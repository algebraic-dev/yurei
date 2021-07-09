module ANSI 

namespace Deco
  public export 
  reset : String 
  reset = "\x1b[0m"

  public export 
  dim : String
  dim = "\x1b[2m"

  public export 
  bold : String 
  bold = "\x1b[1m"

namespace Fg 
  public export 
  cyan : String 
  cyan = "\x1b[34m"

  public export 
  red : String 
  red = "\x1b[31m"

namespace Bg 
  public export 
  red : String 
  red = "\x1b[41m" 