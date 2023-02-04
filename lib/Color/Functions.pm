package Color::Functions;
use warnings;
use strict;
use List::Util qw(min max);
use POSIX qw(round fmod);
use Math::Trig qw(pi2);

=pod

=head1 NAME

Color::Functions - functions for converting between color spaces, etc.

=cut

use base "Exporter";

our @EXPORT = qw();
our @EXPORT_OK = (
    'srgb_to_linear',
    'linear_to_srgb',
    'linear_to_hsv',
    'linear_to_hsl',
    'linear_to_hsi',
    'linear_to_hsp',
    'hsv_to_linear',
    'hsl_to_linear',
    'hsi_to_linear',
    'hsp_to_linear',
    'linear_luminance',
    'linear_color_mix',
    'srgb_color_mix',
    'multiply_255',
    'divide_255',
    'srgb_to_hsl',              # convenience
    'srgb_to_hsv',
    'srgb_to_hsi',
    'srgb_to_hsp',
    'hsl_to_srgb',
    'hsv_to_srgb',
    'hsi_to_srgb',
    'hsp_to_srgb',
    'srgb255_to_hsl',           # even more convenience
    'srgb255_to_hsv',
    'srgb255_to_hsi',
    'srgb255_to_hsp',
    'hsl_to_srgb255',
    'hsv_to_srgb255',
    'hsi_to_srgb255',
    'hsp_to_srgb255',
    'srgb255_hex',
    'srgb_hex',
);
our %EXPORT_TAGS = (all => \@EXPORT_OK);

sub shift3 (\@);

=over 4

=item multiply_255

converts values in the range of [0, 1] to [0, 255].

=cut

sub multiply_255 {
    my ($r, $g, $b) = shift3 @_;
    my @result = map { clamp($_) * 255 } ($r, $g, $b);
    return @result if wantarray;
    return \@result;
}

=item divide_255

converts values in the range of [0, 255] to [0, 1].

=cut

sub divide_255 {
    my ($r, $g, $b) = shift3 @_;
    my @result = map { clamp($_, 0, 255) / 255 } ($r, $g, $b);
    return @result if wantarray;
    return \@result;
}

=item srgb_to_linear

Converts gamma-corrected sRGB color values in the range of [0, 1] to
linear RGB color values in the range [0, 1].

=cut

# https://en.wikipedia.org/wiki/SRGB
sub srgb_to_linear {
    my ($r, $g, $b) = shift3 @_;
    my $linear_r = ($r <= 0.04045) ? ($r / 12.92) : ((($r + 0.055) / 1.055) ** 2.4);
    my $linear_g = ($g <= 0.04045) ? ($g / 12.92) : ((($g + 0.055) / 1.055) ** 2.4);
    my $linear_b = ($b <= 0.04045) ? ($b / 12.92) : ((($b + 0.055) / 1.055) ** 2.4);
    my @result = ($linear_r, $linear_g, $linear_b);
    return @result if wantarray;
    return \@result;
}

=item srgb_to_linear,

Converts linear RGB color values in the range of [0, 1] to
gamma-corrected sRGB color values in the range of [0, 1].  Arguments
and return values are in the range [0, 1].

=cut

# https://en.wikipedia.org/wiki/SRGB
sub linear_to_srgb {
    my ($r, $g, $b) = shift3 @_;
    my $srgb_r = ($r <= 0.0031308) ? ($r * 12.92) : (1.055 * ($r ** (1 / 2.4)) - 0.055);
    my $srgb_g = ($g <= 0.0031308) ? ($g * 12.92) : (1.055 * ($g ** (1 / 2.4)) - 0.055);
    my $srgb_b = ($b <= 0.0031308) ? ($b * 12.92) : (1.055 * ($b ** (1 / 2.4)) - 0.055);
    my @result = ($srgb_r, $srgb_g, $srgb_b);
    return @result if wantarray;
    return \@result;
}

=item linear_to_hsv, linear_to_hsl, linear_to_hsi

Converts linear RGB color space values in [0, 1] to HSV, HSL, or HSI
color space values in [0, 1].

=item linear_to_hsv, linear_to_hsl, linear_to_hsi

Converts HSV, HSL, or HSI color space values in [0, 1] to linear RGB
color space values in [0, 1].

=cut

# https://en.wikipedia.org/wiki/HSL_and_HSV
sub linear_to_hsv {
    my ($r, $g, $b) = shift3 @_;
    my $max = max($r, $g, $b);
    my $min = min($r, $g, $b);
    my $c = $max - $min;
    my $v = $max;
    my $h = linear_hue($r, $g, $b);
    my $s = near_equal($v, 0) ? 0 : ($c / $v);
    return ($h, $s, $v) if wantarray;
    return [$h, $s, $v];
}

# https://en.wikipedia.org/wiki/HSL_and_HSV
sub linear_to_hsl {
    my ($r, $g, $b) = shift3 @_;
    my $max = max($r, $g, $b);
    my $min = min($r, $g, $b);
    my $c = $max - $min;
    my $h = linear_hue($r, $g, $b);
    my $l = ($max + $min) / 2;
    my $s = (near_equal($l, 0) || near_equal($l, 1)) ? 0 : ($c / (1 - abs(2 * $l - 1)));
    return ($h, $s, $l) if wantarray;
    return [$h, $s, $l];
}

# https://en.wikipedia.org/wiki/HSL_and_HSV
sub linear_to_hsi {
    my ($r, $g, $b) = shift3 @_;
    my $max = max($r, $g, $b);
    my $min = min($r, $g, $b);
    my $c = $max - $min;
    my $h = linear_hue($r, $g, $b);
    my $i = ($r + $g + $b) / 3; # intensity (HSI)
    my $s = near_equal($i, 0) ? 0 : (1 - $min / $i);
    return ($h, $s, $i) if wantarray;
    return [$h, $s, $i];
}

# https://alienryderflex.com/hsp.html
sub linear_to_hsp {
    my ($r, $g, $b) = shift3 @_;
    my $p = linear_luminance($r, $g, $b);
    if ($r == $g && $r == $b) {
        return [0, 0, $p];
    }
    my $h;
    my $s;
    if ($r >= $g && $r >= $b) {
        if ($b >= $g) {
            $h = 6/6 - 1/6 * ($b - $g) / ($r - $g);
            $s = 1 - $g / $r;
        } else {
            $h = 0/6 + 1/6 * ($g - $b) / ($r - $b);
            $s = 1 - $b / $r;
        }
    } elsif ($g >= $r && $g >= $b) {
        if ($r >= $b) {
            $h = 2/6 - 1/6 * ($r - $b) / ($g - $b);
            $s = 1 - $b / $g;
        } else {
            $h = 2/6 + 1/6 * ($b - $r) / ($g - $r);
            $s = 1 - $r / $g;
        }
    } else {
        if ($g >= $r) {
            $h = 4/6 - 1/6 * ($g - $r) / ($b - $r);
            $s = 1 - $r / $b;
        } else {
            $h = 4/6 + 1/6 * ($r - $g) / ($b - $g);
            $s = 1 - $g / $b;
        }
    }
    return [$h, $s, $p];
}

# https://en.wikipedia.org/wiki/HSL_and_HSV
sub linear_hue {
    my ($r, $g, $b) = shift3 @_;
    my $max = max($r, $g, $b);
    my $min = min($r, $g, $b);
    my $c = $max - $min;
    my $h = near_equal($c, 0) ? 0 :
      ($max == $r) ? (($g - $b) / $c) :
      ($max == $g) ? (($b - $r) / $c + 2) :
      ($max == $b) ? (($r - $g) / $c + 4) : 0;
    $h /= 6;
    if ($h < 0) { $h += 1; }
    return $h;
}

# https://vocal.com/video/rgb-and-hsvhsihsl-color-space-conversion/
sub hsv_to_linear {
    my ($h, $s, $v) = shift3 @_;
    while ($h < 0) { $h += 1; }
    while ($h >= 1) { $h -= 1; }
    my $c = $v * $s;
    my $x = $c * (1 - abs(fmod($h * 6, 2) - 1));
    my ($r, $g, $b) = h_to_rgb_helper($h, $c, $x);
    my $m = $v - $c;
    $r += $m;
    $g += $m;
    $b += $m;
    return ($r, $g, $b) if wantarray;
    return [$r, $g, $b];
}

# https://vocal.com/video/rgb-and-hsvhsihsl-color-space-conversion/
sub hsl_to_linear {
    my ($h, $s, $l) = shift3 @_;
    while ($h < 0) { $h += 1; }
    while ($h >= 1) { $h -= 1; }
    my $c = (1 - abs(2 * $l - 1)) * $s;
    my $h6 = $h * 6;
    my $x = $c * (1 - abs(fmod($h6, 2) - 1));
    my ($r, $g, $b) = h_to_rgb_helper($h, $c, $x);
    my $m = $l - $c / 2;
    $r += $m;
    $g += $m;
    $b += $m;
    return ($r, $g, $b) if wantarray;
    return [$r, $g, $b];
}

# https://vocal.com/video/rgb-and-hsvhsihsl-color-space-conversion/
sub hsi_to_linear {
    my ($h, $s, $i) = shift3 @_;
    while ($h < 0) { $h += 1; }
    while ($h >= 1) { $h -= 1; }
    my $rad_h = $h * pi2;       # [0, 1] => [0, 2pi]
    my $rad_60  = pi2 * 1/6;    # pi/3
    my $rad_120 = pi2 * 2/6;    # 2pi/3
    my $rad_180 = pi2 * 3/6;    # pi
    my $rad_240 = pi2 * 4/6;    # 4pi/3
    my $rad_300 = pi2 * 5/6;    # 5pi/3
    my $rad_360 = pi2 * 6/6;    # 2pi
    my $h3 = $h * 3;
    my ($r, $g, $b);
    if ($h3 <= 1) {
        $b = $i * (1 - $s);
        $r = $i * (1 + ($s * cos($rad_h) / cos($rad_60 - $rad_h)));
        $g = 3 * $i - $b - $r;
    } elsif ($h3 <= 2) {
        $r = $i * (1 - $s);
        $g = $i * (1 + ($s * cos($rad_h - $rad_120) / cos($rad_180 - $rad_h)));
        $b = 3 * $i - $r - $g;
    } elsif ($h3 <= 3) {
        $g = $i * (1 - $s);
        $b = $i * (1 + ($s * cos($rad_h - $rad_240) / cos($rad_300 - $rad_h)));
        $r = 3 * $i - $b - $g;
    }
    return ($r, $g, $b) if wantarray;
    return [$r, $g, $b];
}

sub h_to_rgb_helper {
    my ($h, $c, $x) = @_;
    my $h6 = $h * 6;
    my ($r, $g, $b);
    if ($h6 <= 1) {
        ($r, $g, $b) = ($c, $x, 0);
    } elsif ($h6 <= 2) {
        ($r, $g, $b) = ($x, $c, 0);
    } elsif ($h6 <= 3) {
        ($r, $g, $b) = (0, $c, $x);
    } elsif ($h6 <= 4) {
        ($r, $g, $b) = (0, $x, $c);
    } elsif ($h6 <= 5) {
        ($r, $g, $b) = ($x, 0, $c);
    } elsif ($h6 <= 6) {
        ($r, $g, $b) = ($c, 0, $x);
    }
    return ($r, $g, $b) if wantarray;
    return [$r, $g, $b];
}

our $Pr;
our $Pg;
our $Pb;
BEGIN {
    $Pr = 0.2126;
    $Pg = 0.7152;
    $Pb = 0.0722;
}

# https://alienryderflex.com/hsp.html
sub hsp_to_linear {
    my ($h, $s, $p) = shift3 @_;
    my ($r, $g, $b);
    my $minOverMax = 1 - $s;
    my $part;
    if ($minOverMax>0) {
        if ( $h<1/6) {   #  R>G>B
            $h = 6*( $h-0/6); $part=1+$h*(1/$minOverMax-1);
            $b=$p/sqrt($Pr/$minOverMax/$minOverMax+$Pg*$part*$part+$Pb);
            $r=($b)/$minOverMax; $g=($b)+$h*(($r)-($b));
        } elsif ( $h<2/6) {   #  G>R>B
            $h = 6*(-$h+2/6); $part=1+$h*(1/$minOverMax-1);
            $b=$p/sqrt($Pg/$minOverMax/$minOverMax+$Pr*$part*$part+$Pb);
            $g=($b)/$minOverMax; $r=($b)+$h*(($g)-($b));
        } elsif ( $h<3/6) {   #  G>B>R
            $h = 6*( $h-2/6); $part=1+$h*(1/$minOverMax-1);
            $r=$p/sqrt($Pg/$minOverMax/$minOverMax+$Pb*$part*$part+$Pr);
            $g=($r)/$minOverMax; $b=($r)+$h*(($g)-($r));
        } elsif ( $h<4/6) {   #  B>G>R
            $h = 6*(-$h+4/6); $part=1+$h*(1/$minOverMax-1);
            $r=$p/sqrt($Pb/$minOverMax/$minOverMax+$Pg*$part*$part+$Pr);
            $b=($r)/$minOverMax; $g=($r)+$h*(($b)-($r));
        } elsif ( $h<5/6) {   #  B>R>G
            $h = 6*( $h-4/6); $part=1+$h*(1/$minOverMax-1);
            $g=$p/sqrt($Pb/$minOverMax/$minOverMax+$Pr*$part*$part+$Pg);
            $b=($g)/$minOverMax; $r=($g)+$h*(($b)-($g));
        } else {   #  R>B>G
            $h = 6*(-$h+6/6); $part=1+$h*(1/$minOverMax-1);
            $g=$p/sqrt($Pr/$minOverMax/$minOverMax+$Pb*$part*$part+$Pg);
            $r=($g)/$minOverMax; $b=($g)+$h*(($r)-($g));
        }
    }
    else {
        if ( $h<1/6) {   #  R>G>B
            $h = 6*( $h-0/6); $r=sqrt($p*$p/($Pr+$Pg*$h*$h)); $g=($r)*$h; $b=0;
        } elsif ( $h<2/6) {   #  G>R>B
            $h = 6*(-$h+2/6); $g=sqrt($p*$p/($Pg+$Pr*$h*$h)); $r=($g)*$h; $b=0;
        } elsif ( $h<3/6) {   #  G>B>R
            $h = 6*( $h-2/6); $g=sqrt($p*$p/($Pg+$Pb*$h*$h)); $b=($g)*$h; $r=0;
        } elsif ( $h<4/6) {   #  B>G>R
            $h = 6*(-$h+4/6); $b=sqrt($p*$p/($Pb+$Pg*$h*$h)); $g=($b)*$h; $r=0;
        } elsif ( $h<5/6) {   #  B>R>G
            $h = 6*( $h-4/6); $b=sqrt($p*$p/($Pb+$Pr*$h*$h)); $r=($b)*$h; $g=0;
        } else {   #  R>B>G
            $h = 6*(-$h+6/6); $r=sqrt($p*$p/($Pr+$Pb*$h*$h)); $b=($r)*$h; $g=0;
        }
    }
    return ($r, $g, $b) if wantarray;
    return [$r, $g, $b];
}

# https://en.wikipedia.org/wiki/Luma_(video)
#
# The linear_luminance function calculates LUMINANCE when used with
# linear R, G, B values.
#
# The srgb_luma function calculates LUMA from gamma-corrected R, G, B
# values.
#
# Both are calculated the same way, though srgb_luma is probably not
# used that often.

=item linear_luminance, srgb_luma

linear_luminance takes the specified linear RGB color component values
in [0, 1] and returns the color's luminance value in [0, 1].

srgb_luma takes the specified gamma-compressed sRGB color component
values in [0, 1] and returns the color's luma value in [0, 1].

The linear_luminance and srgb_luma functions are, in fact, the same
function.  Linear luminance reflects the actual brightness of a color
(specified as linear RGB), while luma is used in video signaling for
something called "chroma subsampling".

=cut

sub linear_luminance {
    my ($r, $g, $b) = shift3 @_;
    return 0.2126 * $r + 0.7152 * $g + 0.0722 * $b;
}
sub srgb_luma {
    goto &linear_luminance;
}

sub near_equal {
    my ($a, $b) = @_;
    return abs($a - $b) < 0.00000000000001;
}

=head1 linear_contrast_ratio

Accepts two colors, each specified as three linear RGB component
values in [0, 1], and returns the contrast ratio between them in the
range [1, 21].  1 means no contrast and 21 means the highest possible
contrast.

=cut

# https://www.accessibility-developer-guide.com/knowledge/colours-and-contrast/how-to-calculate/
# 1  = no contrast (1 to 1)
# 21 = max contrast (21 to 1)
sub linear_contrast_ratio {
    my ($r1, $g1, $b1) = shift3 @_;
    my ($r2, $g2, $b2) = shift3 @_;
    my $y1 = linear_luminance_srgb($r1, $g1, $b1);
    my $y2 = linear_luminance_srgb($r2, $g2, $b2);
    if ($y1 < $y2) {
        ($y1, $y2) = ($y2, $y1);
    }
    return ($y1 + 0.05) / ($y2 + 0.05);
}

=head1 linear_color_mix

Accepts two colors, each specified as three linear RGB component
values in [0, 1], and an opacity in [0, 1].  Returns the result of
mixing the first color with a proportion of the second color
represented by the opacity, as linear RGB component values in [0, 1].
opacity = 0 results in the first color; opacity = 1 results in the
second color.

=cut

sub linear_color_mix {
    my ($r1, $g1, $b1) = shift3 @_;
    my ($r2, $g2, $b2) = shift3 @_;
    my $opacity = shift;
    my $r = $r1 + ($r2 - $r1) * clamp($opacity);
    my $g = $g1 + ($g2 - $g1) * clamp($opacity);
    my $b = $b1 + ($b2 - $b1) * clamp($opacity);
    return ($r, $g, $b) if wantarray;
    return [$r, $g, $b];
}

=head1 srgb_color_mix

Does the same color mixing as the linear_color_mix function.  Accepts
gamma-corrected sRGB color values in [0, 1], converts them to linear,
does the mixing, and returns gamma-corrected sRGB values in [0, 1].

=cut

sub srgb_color_mix {
    my ($r1, $g1, $b1) = shift3 @_;
    my ($r2, $g2, $b2) = shift3 @_;
    my $opacity = shift;
    ($r1, $g1, $b1) = srgb_to_linear($r1, $g1, $b1);
    ($r2, $g2, $b2) = srgb_to_linear($r2, $g2, $b2);
    my ($r, $g, $b) = linear_color_mix($r1, $g1, $b1, $r2, $g2, $b2, $opacity);
    ($r, $g, $b) = linear_to_srgb($r, $g, $b);
    return ($r, $g, $b) if wantarray;
    return [$r, $g, $b];
}

sub clamp {
    my ($x, $min, $max) = @_;
    if (!defined $min && !defined $max) {
        ($min, $max) = (0, 1);
    }
    $min //= '-Inf' + 0;
    $max //= 'Inf' + 0;
    return $x < $min ? $min : $x > $max ? $max : $x;
}

sub hsl_to_srgb    { my ($h, $s, $l) = shift3 @_; return linear_to_srgb(hsl_to_linear($h, $s, $l)); }
sub hsv_to_srgb    { my ($h, $s, $v) = shift3 @_; return linear_to_srgb(hsv_to_linear($h, $s, $v)); }
sub hsi_to_srgb    { my ($h, $s, $i) = shift3 @_; return linear_to_srgb(hsi_to_linear($h, $s, $i)); }
sub hsp_to_srgb    { my ($h, $s, $p) = shift3 @_; return linear_to_srgb(hsp_to_linear($h, $s, $p)); }
sub srgb_to_hsl    { my ($r, $g, $b) = shift3 @_; return linear_to_hsl(srgb_to_linear($r, $g, $b)); }
sub srgb_to_hsv    { my ($r, $g, $b) = shift3 @_; return linear_to_hsv(srgb_to_linear($r, $g, $b)); }
sub srgb_to_hsi    { my ($r, $g, $b) = shift3 @_; return linear_to_hsi(srgb_to_linear($r, $g, $b)); }
sub srgb_to_hsp    { my ($r, $g, $b) = shift3 @_; return linear_to_hsp(srgb_to_linear($r, $g, $b)); }
sub srgb255_to_hsl { my ($r, $g, $b) = shift3 @_; return srgb_to_hsl(divide_255($r, $g, $b)); }
sub srgb255_to_hsv { my ($r, $g, $b) = shift3 @_; return srgb_to_hsv(divide_255($r, $g, $b)); }
sub srgb255_to_hsi { my ($r, $g, $b) = shift3 @_; return srgb_to_hsi(divide_255($r, $g, $b)); }
sub srgb255_to_hsp { my ($r, $g, $b) = shift3 @_; return srgb_to_hsp(divide_255($r, $g, $b)); }
sub hsl_to_srgb255 { my ($h, $s, $l) = shift3 @_; return multiply_255(hsl_to_srgb($h, $s, $l)); }
sub hsv_to_srgb255 { my ($h, $s, $v) = shift3 @_; return multiply_255(hsv_to_srgb($h, $s, $v)); }
sub hsi_to_srgb255 { my ($h, $s, $i) = shift3 @_; return multiply_255(hsi_to_srgb($h, $s, $i)); }
sub hsp_to_srgb255 { my ($h, $s, $p) = shift3 @_; return multiply_255(hsp_to_srgb($h, $s, $p)); }

sub raw_hex {
    my ($r, $g, $b) = shift3 @_;
    return sprintf('#%02x%02x%02x', map { round($_) } $r, $g, $b);
}
sub srgb255_hex {
    my ($r, $g, $b) = shift3 @_;
    return sprintf('#%02x%02x%02x', map { round($_) } $r, $g, $b);
}
sub srgb_hex {
    my ($r, $g, $b) = shift3 @_;
    return sprintf('#%02x%02x%02x', map { round($_) } @{multiply_255($r, $g, $b)});
}

###############################################################################
sub shift3 (\@) {
    my ($array) = @_;
    my $a = shift @$array;
    my @result;
    if (ref $a eq 'ARRAY') {
        @result = @$a[0, 1, 2];
    } else {
        my $b = shift @$array;
        my $c = shift @$array;
        @result = ($a, $b, $c);
    }
    return @result if wantarray;
    return \@result;
}

1;
