//! Arbitrary-precision conversion of digit strings to decimal.

/// Convert `digits`, already checked to be valid in `radix`, to decimal
/// without an upper bound on their value.
pub(crate) fn digits_to_decimal(digits: &str, radix: u32) -> String {
    places_to_decimal(
        digits
            .chars()
            .map(|c| (radix, c.to_digit(radix).expect("digits checked by caller"))),
    )
}

/// Like [`digits_to_decimal`], with a `-` in front when `negative` unless
/// the value is zero.
pub(crate) fn signed_digits_to_decimal(digits: &str, radix: u32, negative: bool) -> String {
    let magnitude = digits_to_decimal(digits, radix);
    if negative && magnitude != "0" {
        format!("-{magnitude}")
    } else {
        magnitude
    }
}

/// Convert a number given as `(radix, digit)` places, most significant
/// first, to decimal without an upper bound on its value. Each place has
/// its own radix, so mixed-radix numbers such as base 60 `1:30` work too.
pub(crate) fn places_to_decimal(places: impl IntoIterator<Item = (u32, u32)>) -> String {
    const LIMB: u64 = 1_000_000_000;
    // Little-endian base 10^9 limbs.
    let mut limbs: Vec<u64> = Vec::new();
    for (radix, digit) in places {
        debug_assert!(digit < radix);
        let mut carry = u64::from(digit);
        for limb in &mut limbs {
            let v = *limb * u64::from(radix) + carry;
            *limb = v % LIMB;
            carry = v / LIMB;
        }
        if carry > 0 {
            limbs.push(carry);
        }
    }
    let Some((most, rest)) = limbs.split_last() else {
        return "0".to_string();
    };
    let mut out = most.to_string();
    for limb in rest.iter().rev() {
        out.push_str(&format!("{limb:09}"));
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_digits_to_decimal() {
        for (digits, radix, expected) in [
            ("", 10, "0"),
            ("000", 8, "0"),
            ("0755", 8, "493"),
            ("1000000000", 10, "1000000000"),
            ("1000000000000000000", 10, "1000000000000000000"),
            ("ffffffffffffffff", 16, "18446744073709551615"),
            (
                "100000000000000000000000000000000",
                16,
                "340282366920938463463374607431768211456",
            ),
        ] {
            assert_eq!(digits_to_decimal(digits, radix), expected, "{digits}");
        }
    }

    #[test]
    fn test_places_to_decimal() {
        assert_eq!(places_to_decimal([(10, 1), (60, 30)]), "90");
        assert_eq!(
            places_to_decimal([
                (10, 1),
                (60, 0),
                (60, 0),
                (60, 0),
                (60, 0),
                (60, 0),
                (60, 0)
            ]),
            "46656000000"
        );
    }
}
