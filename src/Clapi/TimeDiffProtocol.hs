module Clapi.TimeDiffProtocol (TimeDelta, timeDiffProto) where
import Control.Monad (forever)
import Control.Monad.Trans (lift)
import Data.Word (Word32)
import System.Clock (Clock(Monotonic), TimeSpec(..), getTime)

import Clapi.Types (Time(..), TimeStamped(..))
import Clapi.Server (ClientEvent(..), ServerEvent)
import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev)

fromTimeSpec :: TimeSpec -> Time
fromTimeSpec (TimeSpec s ns) = Time
    (fromIntegral s)
    (round $ (toRational (maxBound :: Word32) / 10 ^^ 9) * (toRational ns))

getTimeMonotonic :: IO Time
getTimeMonotonic = fromTimeSpec <$> getTime Monotonic

newtype TimeDelta = TimeDelta Time

timeDelta :: Time -> Time -> TimeDelta
timeDelta (Time s0 f0) (Time s1 f1) = TimeDelta $ if f1 > f0
    then Time (s0 - s1 - 1) ((maxBound :: Word32) - f1 + f0)
    else Time (s0 - s1) (f0 - f1)

timeDiffProto ::
    Monad m =>
    (m Time) ->
    (i -> TimeDelta -> m ()) ->
    (i -> m ()) ->
    Protocol
        (ClientEvent i (TimeStamped a) x)
        (ClientEvent i a x)
        (ServerEvent i b)
        (ServerEvent i b)
        m ()
timeDiffProto getTime storeDelta dropDelta = forever $ waitThen fwd sendRev
  where
    fwd (ClientData i (TimeStamped (theirTime, a))) = do
        ourTime <- lift getTime
        lift $ storeDelta i $ timeDelta ourTime theirTime
        sendFwd $ ClientData i a
    fwd (ClientConnect i x)  = sendFwd $ ClientConnect i x
    fwd (ClientDisconnect i) = do
        lift $ dropDelta i
        sendFwd $ ClientDisconnect i
