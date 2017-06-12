module XMonad.MyConfig.Common
       where
import           XMonad

 {-
/---- meta
|/--- shift
||/-- control
|||
vvv -}
m__, ms_, m_c, msc, m4m :: KeyMask -> KeyMask
m__ = id
ms_ = \m -> m .|. shiftMask
m_c = \m -> m .|. controlMask
msc = \m -> m .|. shiftMask .|. controlMask
m4m = const mod4Mask

