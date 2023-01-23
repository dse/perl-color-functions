package Color::Functions;
use warnings;
use strict;
use List::Util qw(min max);

our $GAMMA = 2.4;

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
        ($_ <= 0.04045) ? ($_ / 12.92) : ((($_ + 0.055) ** 2.4) / 1.055)
    } ($r, $g, $b);
    return ($r, $g, $b) if wantarray;
    return [$r, $g, $b];
}

sub linear_to_srgb {
    @_ = map { (ref $_ eq 'ARRAY') ? @$_ : ($_) } @_;
    my ($r, $g, $b) = map { clamp($_) } @_;
    ($r, $g, $b) = map {
        ($_ <= 0.0031308) ? (12.92 * $_) : (1.055 * ($_ ** (1 / 2.4)) - 0.055)
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

# HDTV
sub linear_luminance_709 {
    @_ = map { (ref $_ eq 'ARRAY') ? @$_ : ($_) } @_;
    my ($r, $g, $b) = map { clamp($_) } @_;
    return 0.2126 * $r + 0.7152 * $g + 0.0722 * $b;
}

# SDTV
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
}

sub near_equal {
    my ($a, $b) = @_;
    return abs($a - $b) < 0.00000000000001;
}

1;
