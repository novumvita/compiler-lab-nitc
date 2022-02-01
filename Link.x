{
module Link (replaceLabels) where
import Data.Either
}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [a-zA-Z]
@label = $alpha [ $alpha $digit ]*
@secondpass = 1

tokens :-
                \"[^\"]*\"                  { notLabel }
                @label ":"\n                { label }
<secondpass>    @label                      { labelAccess }
                .|\n                        { notLabel }
  

{

type AlexUserState = (String, [(String, Int)])

alexEOF = return ()

alexInitUserState :: AlexUserState
alexInitUserState = ("", [])

getUserState ::  Alex AlexUserState
getUserState = Alex $ \s -> Right (s,alex_ust s)

modifyUserState :: (AlexUserState -> AlexUserState) -> Alex ()
modifyUserState f = Alex $ \s -> Right (s { alex_ust = (f $ alex_ust s) },())

runAlexScan :: String -> Either String AlexUserState
runAlexScan s = runAlex s $ alexMonadScan >> getUserState

fixOffset :: (Int, (String, Int)) -> (String, Int)
fixOffset (fix, (label, line)) = (label, line - 2 * fix)

fixOffsets :: [(String, Int)] -> [(String, Int)]
fixOffsets = zipWith (curry fixOffset) [0..]

alexRescan input map = case f (AlexState {
  alex_bytes = [],
  alex_pos = alexStartPos,
  alex_inp = input,
  alex_chr = '\n',
  alex_scd = 1,
  alex_ust = ("", map)
}) of 
  Left msg -> error "Linking Failed"
  Right ( _, (newCode, _) ) -> newCode
  where
    Alex f = alexMonadScan >> getUserState

    
label :: AlexAction()
label ((AlexPn _ line _), _, _, s) len = modifyUserState (\(code, map) -> (code, map ++ [(label, addr)])) >> alexMonadScan
    where
    label = take (len-2) s
    addr = 2056 + 2 * (line - 9)

labelAccess :: AlexAction()
labelAccess (_, _, _, s) len = modifyUserState (replaceLabel old) >> alexMonadScan
    where
    old = take len s
    replaceLabel :: String -> AlexUserState -> AlexUserState
    replaceLabel label (code, map) = (code ++ new, map)
      where
        key = label
        new = case (lookup key map) of
          Nothing -> label
          Just addr -> show addr

notLabel :: AlexAction()
notLabel (_, _, _, s) len = modifyUserState (\(code, map) -> (code ++ new, map)) >> alexMonadScan   
  where
    new = take len s

replaceLabels :: String -> IO String
replaceLabels code = do
print fixedMap
return replacedCode
  where
    (codeNew , map) = fromRight (error "Linking failed") (runAlexScan code)
    fixedMap = fixOffsets map
    replacedCode = alexRescan codeNew fixedMap

}