import Data.List.Extra (chunksOf)

type Qreg = String

data Inst =
    ZGate Qreg
    | Reflect0 Qreg 
    | Unitary String Qreg 
    | Adj Inst

instance Show Inst where
    show (ZGate q) = "z " <> q
    show (Reflect0 q) = "reflect_0 " <> q
    show (Unitary u q) = unwords [u, q]
    show (Adj i) = "inv_" <> show i

type Prog = [Inst]

class Adjoint a where 
    adjoint :: a -> a

dagger :: Char
dagger = '†'

instance Adjoint String where
    adjoint s
        | last s == dagger = init s
        | otherwise = s <> (dagger:"")

instance Adjoint Inst where
    adjoint i@(ZGate _) = i
    adjoint i@(Reflect0 _) = i
    adjoint (Adj i) = i
    adjoint i = Adj i

instance Adjoint Prog where
    adjoint = map adjoint . reverse

amplify :: Int -> Prog -> Prog
amplify 0 alg = alg
amplify 1 alg = alg ++ [ZGate "r"] ++ (adjoint alg) ++ [Reflect0 "q, r"] ++ alg
amplify k alg = amplify 1 (amplify (k - 1) alg)

amp1 :: Prog -> Prog
amp1 = amplify 1

vtamplify :: Int -> Prog -> Prog -> Prog
vtamplify 0 circ alg = alg
vtamplify j circ alg = amp1 (vtamplify (j - 1) circ alg) ++ [circ !! (3^j)]

showQASM :: Prog -> String
showQASM = unlines . map ((<> ";") . show)

main :: IO ()
main = do
    let alg = [Unitary "uniform" "q", Unitary "oracle" "q, r"]
    let alg = [Unitary "alg_A" "q, r"]
    let circ = [Unitary ("C(" <> show i <> ")") "q, r" | i <- [1..]]
    let p = vtamplify 1000 circ alg
    let pp = take 40 p
    putStrLn $ showQASM pp

