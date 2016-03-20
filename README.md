Requires 1.9 nightlies for char_at().

old eval:
      /*if let Sexps::SubSexps(box v) = *sexps {
         match &v[..] {
            [] => Sexps::Err(String::from("Trying to evaluate empty list")),
            [ref x] => self.apply(self.eval(&x), lst_new::<Sexps>()),
            [ref x, ref xs..] => self.apply(self.eval(&x), vec_to_lst::<Sexps>(&hack(&xs))),
         }
      }
      else { *sexps } //we're an atom*/

