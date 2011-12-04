
\begin{code}
module Text.Printf.TH (printf) where

-- import Text.PrettyPrint (render)
import Language.Haskell.TH
import Text.Printf.TH.Parser (parse)
import Text.Printf.TH.Types
import Text.Printf.TH.Simplify (simplify)

{-
xn where n is an integer refers to an argument to the function
y*, n* is reserved for %n
fw* is reserved for field width intermediates
Everything else can be used by the conversion functions
-}

{-
printf takes a (format) string and returns a function that prints as it
describes.
-}
printf :: String -> ExpQ
printf s = do -- qIO $ putStrLn $ "Doing " ++ s
              let (formats, max_x_var) = parse s
                  x_vars = (map (varP . mkName . xvar) [1..max_x_var])
                  dec_n0 = valD (varP $ mkName $ nvar 0) (normalB [| 0 |]) []
              -- qIO $ putStrLn $ show (formats, max_x_var)
              e <- lamE x_vars (combine [dec_n0] [| showString "" |] 1 formats)
              -- qIO $ putStrLn $ render $ pprExp e
              return $ simplify e

combine :: [DecQ]   -- List of declarations we will need at the end
        -> ExpQ     -- The expression we are currently building
        -> ArgNum   -- The next declaration number to be used
        -> [Format] -- The list of formats left to deal with
        -> ExpQ     -- The final expression to splice in
combine decls building_exp y_num []
    = letE decls (tupE (e:(map (varE . mkName . nvar) [1..y_num-1])))
  where e = appE (foldr appE building_exp (map (varE . mkName . yvar) [1..y_num-1]))
                 [| "" |]
combine decls building_exp y_num (CharCount:fs)
    = combine (decl_y:decl_n:decls) [| showString "" |] (y_num + 1) fs
  where decl_y = valD (varP ( mkName $ yvar y_num)) (normalB building_exp') []
        decl_n = valD (varP ( mkName $ nvar y_num)) (normalB length_exp) []
        building_exp' = [| ($building_exp .) |]
        length_exp = [| $(              varE ( mkName $ nvar (y_num - 1)))
                            + length ($(varE ( mkName $ yvar y_num)) (showString "") "") |]
combine decls building_exp y_num (Literal s:fs)
    = combine decls [| $building_exp . showString s |] y_num fs
combine decls building_exp y_num (Conversion e:fs)
    = combine decls [| $building_exp . showString $e |] y_num fs
\end{code}

