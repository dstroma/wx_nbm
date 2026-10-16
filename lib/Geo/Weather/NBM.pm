use v5.44;
use experimental 'class';
no warnings 'experimental::signature_named_parameters';

class Geo::Weather::NBM {
  use builtin ':5.40';
  use Carp qw(croak);

  field $text    :param :reader;
  field $station :param :reader;

  field $is_parsed      = false;
  field $is_meta_parsed = false;
  field $data = {};

  ADJUST {
    croak 'Station is not valid'  unless defined $station and length $station <= 6 and $station =~ m/^\w{2,6}$/;
    croak 'Forecast text invalid' unless defined $text    and length $text > 100;
    $station = uc $station;
  }

  method is_parsed () {
    $is_parsed;
  }

  method data () {
    $self->parse unless $is_parsed;
    $data;
  }

  method meta () {
    $self->parse(meta_only => true) unless $is_meta_parsed;
    $data;
  }

  method parse (:$meta_only = undef) {
    die 'Already parsed' if $is_parsed;

    my @lines = split /\n/, $text;
    shift @lines while (!length $lines[0] or $lines[0] !~ m/$station/);
    croak "Could not find report for $station in text" unless @lines;

    parse_head(\@lines, $data) unless $is_meta_parsed;
    $is_meta_parsed = true;
    return if $meta_only;

    parse_body(\@lines, $data);
    parse_days(\@lines, $data);
    munge($data);

    $is_parsed = true;
  }

  sub parse_head ($lines, $data) {
    # Parse main header
    my $header = $lines->[0];
    my ($sta, $label, $month, $day, $year, $hour, $minute) = $header =~
      m#(\w\w\w\w)\s+(NBM.+NBS GUIDANCE)\s+(\d{1,2})/(\d{1,2})/(\d\d\d\d)\s+(\d\d)(\d\d) UTC#;

    croak "Cannot parse NBS forecast: invalid header:\n\t[$header]\n"
      unless $sta and $label and $month and $day and $year;

    # Get the state, forecast type, and datetime the forecast was generated
    $data->{station}       = $sta;
    $data->{forecast_type} = $label;
    $data->{generated_at}  = DateTimeX::Inflatable->new(
      year => $year, month => $month, day => $day, hour => $hour, minute => $minute, second => 0, time_zone => 'UTC'
    );

    # Using a for loop so we don't copy the array
    my $gen_dt = $data->{generated_at}->inflate;
    my ($beg_hrs, $end_hrs);
    for (my $i = 0; $i <= $#$lines; $i++) {
      if ($lines->[$i] =~ m/^\s*FHR\s*(\d\d).+(\d\d)\s*$/) {
        $beg_hrs = $1;
        $end_hrs = $2;
        last;
      }
    }

    croak 'Problem parsing forecast: cannot determine start and end times'
      unless defined $beg_hrs and $end_hrs;

    my $begin = DateTimeX::Inflatable->deflate($gen_dt->clone->add(hours => $beg_hrs));
    my $end   = DateTimeX::Inflatable->deflate($gen_dt->clone->add(hours => $end_hrs));
    $data->{forecast_period} = { begin => $begin, end => $end };

    return;
  }

  sub parse_days ($lines, $data) {
    # Add UTC_mon and UTC_day and hour_span to each column value
    # This is more difficult than it should be due to the odd formatting
    my $columns = $data->{'columns'};
    my $gen_dt  = $data->{'generated_at'}->inflate;

    foreach my $i (0 .. $#$columns) {
      my $col = $columns->[$i];

      # The easiest way to get the date for this column is to use the
      # forecast cycle date time and add the forecast hour (FHR).
      my $for_dt = $gen_dt->clone->add(hours => $col->{FHR});
      my $m = $col->{'UTC_mon'} = $for_dt->strftime('%m');
      my $d = $col->{'UTC_day'} = $for_dt->strftime('%d');
      my $h = $col->{'UTC'};
      $col->{'UTC_dt'} = DateTimeX::Inflatable->deflate($for_dt);

      # Save to table for conversion from date to array index
      $data->{mdh_to_index}{sprintf('%02d-%02dT%02d', $m, $d, $h)} = $i;

      # Update previous column with hour length (normally 3 but just in case)
      unless ($i == 0) {
        my $span_hours = $columns->[$i]{'UTC'} - $columns->[$i-1]{'UTC'};
        $span_hours += 24 if $span_hours < 0;
        $columns->[$i - 1]->{'hour_span'} = $span_hours;
      }
    }
    return;
  }

  sub parse_body ($lines, $data) {
    my @columns = ();
    for my $line (@$lines) {
      next unless $line =~ m#^\s\w\w\w\s#;
      my $label = substr($line, 1, 3);
      my $rest  = substr($line, 5);
      {
        my $i = 0;
        while ($rest and length $rest >= 3) {
          my $cell_value = substr($rest, 0, 3);
          $rest = substr($rest, 3);
          $columns[$i] //= {};
          $columns[$i]->{$label} = trim($cell_value);
          $i++;
        } #while
      } #scope
    } #for

    $data->{'columns'} = \@columns;
    return;
  } #sub

  sub munge ($data) {
    # The preciptation are keyed for the ending hour. Move them to starting hour
    my $columns = $data->{'columns'};
    foreach my $i (0 .. $#$columns) {
      my $col = $data->{'columns'}[$i];

      # Shift precipitation predictions left
      for my $k (qw(P06 Q06 T06 S06 I06 P12 Q12 T12)) {
        my ($hour_span, $column_span) = (substr($k,1,2) eq '06' ? (6,2) : (12,4));
        if (exists $col->{$k} and length $col->{$k}) {
          my $start_index = $i - $column_span + 1;
          while ($start_index < 0) {
            $start_index++;
            $column_span--;
            $hour_span = $hour_span - 3;
          }
          $columns->[$start_index]{$k . '_start'} = {
            hour_span   => $hour_span,
            column_span => $column_span,
            value       => int($col->{$k} || 0)
          };
          $columns->[$_]{lc($k . 'x')} = int($col->{$k} || 0) for $start_index .. $i;
        }
      }

      # Relative Humidity, Precip Types, Clouds, English Wind Direction
      $col->{'rel_hum'}      = relative_humidity($col->{'TMP'}, $col->{'DPT'});
      $col->{'precip_types'} = precip_types($col->{'PRA'}, $col->{'PRZ'}, $col->{'PPL'}, $col->{'PSN'});
      $col->{'SKY_cld'}      = sky_cover_to_abbr($col->{'SKY'} / 100);
      $col->{'WDR_abbr'}     = wind_direction_abbr($col->{WDR} * 10);
      if (defined $col->{IFV} and defined $col->{IFC}) {
        $col->{'prob_ifr'}     = ($col->{IFV} > $col->{IFC}) ? $col->{IFV} : $col->{IFC};
      }

      # Calculate flight conditions
      my $fr = 'VFR';
      $fr = 'CAVU' if $col->{VIS} >= 100 and $col->{CIG} == -88 and ($col->{LCB} == -88 or $col->{LCB} >= 180);
      $fr = 'MVFR' if $col->{VIS} <= 50 or (0 <= $col->{CIG} < 30);
      $fr = 'IFR'  if $col->{VIS} <  30 or (0 <= $col->{CIG} < 10);
      $fr = 'LIFR' if $col->{VIS} <  10 or (0 <= $col->{CIG} <  5);
      $columns->[$i]{flight_rule} = $fr;

      # Calculate flight conditions - ceiling only
      $col->{CIG_fr} = 'VFR';
      $col->{CIG_fr} = 'MVFR'  if 10 <= $col->{CIG} < 30;
      $col->{CIG_fr} = 'IFR'   if  5 <= $col->{CIG} < 10;
      $col->{CIG_fr} = 'LIFR'  if  0 <= $col->{CIG} < 5;
    }
    return;
  }

  sub relative_humidity ($temp, $dewp) {
    $temp = ($temp - 32) / 1.8;
    $dewp = ($dewp - 32) / 1.8;
    return int(100*(exp((17.625*$dewp)/(243.04+$dewp))/exp((17.625*$temp)/(243.04+$temp))));
  }

  sub precip_types ($ra //= 0, $fr //= 0, $pl //= 0, $sn //= 0) {
    my $total = $ra + $fr + $pl + $sn;
    my @types = ();
    push @types, { name => 'Rain',    prob => $ra } if $ra > $total/10;
    push @types, { name => 'Snow',    prob => $sn } if $sn > $total/10;
    push @types, { name => 'FzRain',  prob => $fr } if $fr > $total/10;
    push @types, { name => 'Sleet',   prob => $pl } if $pl > $total/10;
    @types = sort { $b->{prob} <=> $a->{prob} } @types;
    return \@types;
  }

  sub wind_direction_abbr ($dir) {
    return 'N'   if $dir < 15;
    return 'NNE' if $dir < 30;
    return 'NE'  if $dir < 60;
    return 'ENE' if $dir < 75;
    return 'E'   if $dir < 105;
    return 'ESE' if $dir < 120;
    return 'SE'  if $dir < 150;
    return 'SSE' if $dir < 165;
    return 'S'   if $dir < 195;
    return 'SSW' if $dir < 210;
    return 'SW'  if $dir < 240;
    return 'WSW' if $dir < 255;
    return 'W'   if $dir < 285;
    return 'WNW' if $dir < 300;
    return 'NW'  if $dir < 330;
    return 'NNW' if $dir < 345;
    return 'N';
  }

  sub sky_cover_to_abbr ($pct) {
    my $word;
    $word = 'OVC';                  # Officially 8/8
    $word = 'BKN' if $pct <  7.5/8; # Officially 5/8 to 7/8
    $word = 'SCT' if $pct <  4.5/8; # Officially 3/8 to 4/8, using midpoint of 4/8 and 5/8
    $word = 'FEW' if $pct <= 2  /8; # Officially 1/8 to 2/8
    $word = 'CLR' if $pct <= 1/32;
    return $word;
  }

} #class

package DateTimeX::Inflatable {
  # Stores data necessary to create a DateTime object as an arrayref
  sub new     ($class, @args) { bless \@args, $class;     }
  sub clone   ($self)         { __PACKAGE__->new(@$self); }
  sub to      ($self, $class) { $class->new(@$self);      }
  sub inflate ($self)         { DateTime->new(@$self);    }
  sub deflate ($class, $obj)  {
    my %h;
    foreach my $field (qw(year month day hour minute second time_zone)) {
      $h{$field} = $obj->$field;
    }
    bless [%h], $class;
  }
}

=pod

Synopsis

    my $wx = Geo::Weather::NBM->new(text => $text);
    my $data = $wx->data;

Text Report Key

    DT (or blank) = forcast valid date (in UTC)
    UTC = forcast valid hour (in UTC)
    FHR = model forecast hour (number of hours forward from forecast date/cycle)
    TXN = 18-hour maximum and minimum temperatures, degrees F.
        Min is between 00Z-18Z and reported at 12Z (except Guam)
        Max is between 12z(current day)-06Z(next day) and reported at 00z(following day) (except Guam)
        Guam stations report Tmax at 12z and Tmin at 00z.
    XND = standard deviation of maximum or minimum temperature, degrees F
    TMP = temperature, degrees F
    TSD = standard deviation of temperature, degrees F
    DPT = dew point temperature, degrees F
    DSD = dew point temperature standard deviation, degrees F
    SKY = sky cover, percent
    SSD = sky cover standard deviation, percent
    WDR = wind direction, nearest tens degrees (northerly=36; easterly=9; calm=00)
        oceanic-only stations only have WDR for cycles=0,7,12,19
    WSP = wind speed, knots (<0.5 knots will be listed as 0 (calm))
    WSD = wind speed standard deviation, knots
    GST = wind gust, knots
    GSD = wind gust standard deviation, knots
    P06 = 6-hour PoP, percent
    P12 = 12-hour PoP, percent
    Q06 = 6-hour QPF, 1/100 inches
    Q12 = 12-hour QPF, 1/100 inches
    DUR = Duration of precipitation, hours
    T03 = 3-hour thunderstorm probability, percent
    T06 = 6-hour thunderstorm probability, percent
    T12 = 12-hour thunderstorm probability, percent
    PZR = conditional probability of freezing rain, percent
    PSN = conditional probability of snow, percent
    PPL = conditional probability of sleet / ice pellets, percent
    PRA = conditional probability of rain, percent
    S06 = 6-hour snow amount, 1/10 inches
    SLV = snow level, 100s feet MSL (rounded to nearest 1000 ft for values > 10,000 ft)
    I06 = 6-hour ice amount, 1/100 inches
    CIG = ceiling height, 100s feet (>5000 ft reported to the nearest 1000 ft; -88=unlimited)
    IFC = probability of ceiling IFR flight conditions (ceiling < 1000 ft), percent
    LCB = lowest cloud base, 100s feet (>5000 ft reported to the nearest 1000 ft; -88=unlimited)
    VIS = visibility, 1/10th miles up to 10 miles (rounded to nearest mile for values >= 1 mile; VIS > 10 miles = 100)
    IFV = probability of visibility IFR flight conditions (visibility < 3 miles), percent
    MHT = mixing height, 100s feet AGL
    TWD = transport wind direction, nearest tens degrees (northerly=36; easterly=9; calm=00)
    TWS = transport wind speed, knots (<0.5 knots will be listed as 0 (calm))
    HID = Haines Index (unitless)
    SOL = instantaneous solar radiation, 10s W/m2 (ex: 8=80 W/m2; non-zero values < 10 = 1)
    SWH = significant wave height, feet (marine and some near-water stations only)
