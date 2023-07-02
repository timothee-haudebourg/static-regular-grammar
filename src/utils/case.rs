use std::fmt;

pub struct SnakeCase<'a>(pub &'a str);

impl<'a> fmt::Display for SnakeCase<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut separate = false;
		for (i, c) in self.0.chars().enumerate() {
			match c {
				' ' | '.' | '_' | '-' => separate = true,
				c => {
					if c.is_uppercase() && i > 0 {
						separate = true
					}

					if separate {
						write!(f, "_")?;
						separate = false
					}

					c.to_lowercase().next().unwrap().fmt(f)?
				}
			}
		}

		Ok(())
	}
}

impl<'a> quote::IdentFragment for SnakeCase<'a> {
	fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
		fmt::Display::fmt(self, f)
	}
}
