package Color::Functions;
use warnings;
use strict;
use List::Util qw(min max);

our $GAMMA_2020    = 2.2;
our $ALPHA_2020    = 1.099;           # 1.099296826809442
our $A_2020        = $ALPHA_2020 - 1; # 0.099
our $BETA_2020     = 0.018;           # 0.018053968510807
our $THINGY_2020   = 4.5;
our $THINGY_2_2020 = $THINGY_2020 * $BETA_2020; # 0.081...

our $GAMMA_709     = 2.2;
our $ALPHA_709     = 1.099;
our $A_709         = $ALPHA_709 - 1; # 0.099
our $BETA_709      = 0.018;
our $THINGY_709    = 4.5;
our $THINGY_2_709  = $THINGY_709 * $BETA_709; # 0.081

our $GAMMA_SRGB    = 2.4;
our $ALPHA_SRGB    = 1.055;
our $A_SRGB        = $ALPHA_SRGB - 1; # 0.055
our $BETA_SRGB     = 0.0031308;
our $THINGY_SRGB   = 12.92;                     # orig 12.9232102
our $THINGY_2_SRGB = $THINGY_SRGB * $BETA_SRGB; # 0.04045 orig 0.0392857 from obsolete early draft

sub clamp {
    my ($x, $min, $max) = @_;
    if (!defined $min && !defined $max) {
        ($min, $max) = (0, 1);
    } else {
        $min //= (0 + "-Inf");
        $max //= (0 + "Inf");
    }
    return $min if $x < $min;
    return $max if $x > $max;
    return $x;
}

sub srgb_to_linear {
    @_ = map { (ref $_ eq 'ARRAY') ? @$_ : ($_) } @_;
    my ($r, $g, $b) = map { clamp($_) } @_;
    ($r, $g, $b) = map {
        ($_ <= $THINGY_2_SRGB) ? ($_ / $THINGY_SRGB) : ((($_ + $A_SRGB) ** $GAMMA_SRGB) / $ALPHA_SRGB)
    } ($r, $g, $b);
    return ($r, $g, $b) if wantarray;
    return [$r, $g, $b];
}

sub linear_to_srgb {
    @_ = map { (ref $_ eq 'ARRAY') ? @$_ : ($_) } @_;
    my ($r, $g, $b) = map { clamp($_) } @_;
    ($r, $g, $b) = map {
        ($_ <= $BETA_SRGB) ? ($THINGY_SRGB * $_) : ($ALPHA_SRGB * ($_ ** (1 / $GAMMA_SRGB)) - $A_SRGB)
    } ($r, $g, $b);
    return ($r, $g, $b) if wantarray;
    return [$r, $g, $b];
}

sub mix {
    @_ = map { (ref $_ eq 'ARRAY') ? @$_ : ($_) } @_; # [r,g,b],[r,g,b],amount => r,g,b,r,g,b,amount
    my ($r1, $g1, $b1, $r2, $g2, $b2, $amount) = @_;
    ($r1, $g1, $b1, $r2, $g2, $b2) = map { clamp($_) } ($r1, $g1, $b1, $r2, $g2, $b2);
    my $r = clamp($r1 + ($r2 - $r1) * $amount);
    my $g = clamp($g1 + ($g2 - $g1) * $amount);
    my $b = clamp($b1 + ($b2 - $b1) * $amount);
    return ($r, $g, $b) if wantarray;
    return [$r, $g, $b];
}

# https://en.wikipedia.org/wiki/Luma_(video)
# luma is the weighted sum of gamma-compressed R′G′B′ components
# relative luminance is the weighted sum of linear RGB components

# ITU BT.709 --- HDTV
sub linear_luminance_709 {
    @_ = map { (ref $_ eq 'ARRAY') ? @$_ : ($_) } @_;
    my ($r, $g, $b) = map { clamp($_) } @_;
    return 0.2126 * $r + 0.7152 * $g + 0.0722 * $b;
    # 0.212655; 0.715158; 0.072187
}

# ITU BT.601 --- SDTV
sub linear_luminance_601 {
    @_ = map { (ref $_ eq 'ARRAY') ? @$_ : ($_) } @_;
    my ($r, $g, $b) = map { clamp($_) } @_;
    return 0.299 * $r + 0.587 * $g + 0.114 * $b;
}

# UHDTV, HDR
sub linear_luminance_2020 {
    @_ = map { (ref $_ eq 'ARRAY') ? @$_ : ($_) } @_;
    my ($r, $g, $b) = map { clamp($_) } @_;
    return 0.2627 * $r + 0.6780 * $g + 0.0593 * $b;
}

# Adobe?
# SMPTE RP 145 primaries
# SMPTE C
sub linear_luminance_240 {
    @_ = map { (ref $_ eq 'ARRAY') ? @$_ : ($_) } @_;
    my ($r, $g, $b) = map { clamp($_) } @_;
    return 0.212 * $r + 0.701 * $g + 0.087 * $b;
}

sub linear_hue {
    @_ = map { (ref $_ eq 'ARRAY') ? @$_ : ($_) } @_;
    my ($r, $g, $b) = map { clamp($_) } @_;

    my $max = max($r, $g, $b);
    my $min = min($r, $g, $b);
    my $c = $max - $min;        # range
    my $h;
    if (near_equal($c, 0)) {
        return 0;               # meaningless
    }
    if ($max == $r) {
        if ($g - $b < 0) {
            $h = ($g - $b) / $c + 6;
        } else {
            $h = ($g - $b) / $c;
        }
    } elsif ($max == $g) {
        $h = ($b - $r) / $c + 2;
    } elsif ($max == $b) {
        $h = ($r - $g) / $c + 4;
    }
    return $h / 6;
}

sub linear_saturation_hsv {
    @_ = map { (ref $_ eq 'ARRAY') ? @$_ : ($_) } @_;
    my ($r, $g, $b) = map { clamp($_) } @_;

    # eh?
}

sub linear_saturation_hsl {
    @_ = map { (ref $_ eq 'ARRAY') ? @$_ : ($_) } @_;
    my ($r, $g, $b) = map { clamp($_) } @_;

    my $max = max($r, $g, $b);
    my $min = min($r, $g, $b);
    my $c = $max - $min;        # range
    if (near_equal($c, 0)) {
        return 0;               # meaningless
    }
    my $s;
    my $l = ($max + $min) / 2.0;
    my $delta = $max - $min;
    if ($l < 0.5) {
        $s = $delta / ($max + $min);
    } else {
        $s = $delta / (2.0 - $max - $min);
    }
    return $s;
}

# HSL
sub linear_lightness {
    @_ = map { (ref $_ eq 'ARRAY') ? @$_ : ($_) } @_;
    my ($r, $g, $b) = map { clamp($_) } @_;
    return (min($r, $g, $b) + max($r, $g, $b)) / 2;
}

# HSV
sub linear_value {
    goto &linear_brightness;
}
sub linear_brightness {
    @_ = map { (ref $_ eq 'ARRAY') ? @$_ : ($_) } @_;
    my ($r, $g, $b) = map { clamp($_) } @_;
    return max($r, $g, $b);
}

sub linear_chrominance {
    @_ = map { (ref $_ eq 'ARRAY') ? @$_ : ($_) } @_;
    my ($r, $g, $b) = map { clamp($_) } @_;
    # eh?
}

# https://stackoverflow.com/questions/596216/formula-to-determine-perceived-brightness-of-rgb-color
sub luminance_to_lightness {
    @_ = map { (ref $_ eq 'ARRAY') ? @$_ : ($_) } @_;
    my ($y) = map { clamp($_) } @_;

    if ($y <= 0.00856) {        # 216/24389
        return $y * 903.3;      # 24389/27
    } else {
        return $y ** (1/3) * 116 - 16;
    }
    # L* = 50 when Y = 18.4 or 18% gray card
}

sub contrast_ratio {
    @_ = map { (ref $_ eq 'ARRAY') ? @$_ : ($_) } @_;
    my ($r1, $g1, $b1, $r2, $g2, $b2) = map { clamp($_) } @_;
    my $y1 = linear_luminance_srgb($r1, $g1, $b1);
    my $y2 = linear_luminance_srgb($r2, $g2, $b2);
    if ($y1 < $y2) {
        ($y1, $y2) = ($y2, $y1);
    }
    return ($y1 + 0.05) / ($y2 + 0.05);
    # 1 = no contrast
    # 21 = max contrast
}

sub near_equal {
    my ($a, $b) = @_;
    return abs($a - $b) < 0.00000000000001;
}

1;
