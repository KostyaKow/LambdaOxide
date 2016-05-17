use gentypes::{SharedMut, to_shared_mut};
use errors::{StackInfo};

struct Runner {
   stack_info : SharedMut<StackInfo>,
}

impl Runner {
   fn new() -> Runner {
      Runner {
         stack_info : to_shared_mut(StackInfo::new()),
      }
   }

}
