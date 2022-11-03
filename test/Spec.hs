import Test.Hspec
import Test.Hspec.Expectations

import Data.Text (Text)
import Text.Megaparsec

import Expr
import Parse
import Eval
import qualified Data.Text.IO as T
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = hspec do
  testNumLit
  testPlus
  testApplications
  testInfixes
  testParseExpr
  testBinding
  testEval
  testTestPrograms

testBinding = describe "binding" do
  it "parses x = 1" do
    parseMaybe binding "x = 1" `shouldBe` Just (Var "X", LitInt 1)

testTestPrograms = describe "test programs" do
  it "executes test1.lang correctly" do
    contents <- liftIO $ T.readFile "test_programs/test1.lang"
    let result = runProgram <$> parse bindings "test1.lang" contents
    result `shouldBe` Right (Right $ VI 2)

testNumLit = describe "numLit" do
  it "parses 1" do
    parseMaybe numLit "1" `shouldBe` Just 1

testPlus = describe "plus" do
  it "parses +" do
    let f = parseMaybe plus "+"

    (\f -> f (LitInt 1) (LitInt 1)) <$> f
      `shouldBe` Just (Plus (LitInt 1) (LitInt 1))

  it "parses + followed by spaces" do
    let f = parseMaybe plus "+  "

    (\f -> f (LitInt 1) (LitInt 1)) <$> f
      `shouldBe` Just (Plus (LitInt 1) (LitInt 1))

  it "parses x + 1" do
    parse (do variable
              plus
              numLit) "test" "x + 1" `shouldBe` Right 1

testApplications = describe "applications" do
  it "parses x y" do
    parse applications "test" "x y" `shouldBe` Right (App (V $ Var "x") (V $ Var "y"))
    
testInfixes = describe "infixes" do
  it "parses 1 + 1"  do
    parse infixes "test" "1 + 1" `shouldBe` Right (Plus (LitInt 1) (LitInt 1))

testParseExpr = describe "parseExpr" do
  it "fails to parse 1 +" do
    parseExpr "1 +" `shouldBe` Nothing

  it "parses 1 + 1" do
    parseExpr "1 + 1" `shouldBe` Just (Plus (LitInt 1) (LitInt 1))

  it "parses 1 + 2" do
    parseExpr "1 + 2" `shouldBe` Just (Plus (LitInt 1) (LitInt 2))

  it "parses 1 + x" do
    parseExpr "1 + x" `shouldBe` Just (Plus (LitInt 1) (V $ Var "x"))

  it "parses x+1" do
    parseExpr "x+1" `shouldBe` Just (Plus (V $ Var "x") (LitInt 1))

  it "parses x+ 1" do
    parseExpr "x+ 1" `shouldBe` Just (Plus (V $ Var "x") (LitInt 1))

  it "parses x +1" do
    parseExpr "x +1" `shouldBe` Just (Plus (V $ Var "x") (LitInt 1))

  it "parses x + 1" do
    parseExpr "x + 1" `shouldBe` Just (Plus (V $ Var "x") (LitInt 1))

  it "parses x + y" do
    parseExpr "x + y" `shouldBe` Just (Plus (V $ Var "x") (V $ Var "y"))

  it "parses |x->x+1" do
    parseExpr "|x->x+1" `shouldBe` Just (Lam (Var "x") (Plus (V $ Var "x") (LitInt 1)))

  it "parses | x -> x+1" do
    parseExpr "| x -> x+1" `shouldBe` Just (Lam (Var "x") (Plus (V $ Var "x") (LitInt 1)))

  it "parses | x -> x + 1" do
    parseExpr "| x -> x + 1" `shouldBe` Just (Lam (Var "x") (Plus (V $ Var "x") (LitInt 1)))

  it "parses (|x->x+1)" do
    parseExpr "(|x->x+1)" `shouldBe` Just (Lam (Var "x") (Plus (V $ Var "x") (LitInt 1)))

  it "parses ( | x -> x + 1 )" do
    parseExpr "( | x -> x + 1 )" `shouldBe` Just (Lam (Var "x") (Plus (V $ Var "x") (LitInt 1)))

  it "parses 1 2" do
    parse expr "test" "1 2" `shouldBe` Right (App (LitInt 1) (LitInt 2))

  it "parses (| x -> x + 1) 1" do
    parseExpr "(| x -> x + 1) 1" `shouldBe` Just ( 
      App (Lam (Var "x") (Plus (V $ Var "x") (LitInt 1) ))
          (LitInt 1))



parseEval :: Text -> Maybe (Either RuntimeError Value)
parseEval t = evalIt <$> parseMaybe expr t

testEval = describe "eval" do
  it "evaluates 1+1" do
    parseEval "1+1" `shouldBe` Just (Right $ VI 2)

  it "evaluates 1 + 1" do
    parseEval "1 + 1" `shouldBe` Just (Right $ VI 2)

  it "evaluates an application of a simple lambda" do
    parseEval "(| x -> x + 1) 1" `shouldBe` Just (Right $ VI 2)

  it "gives a runtime error for unknown variable" do
    parseEval "x 1" `shouldBe` Just (Left $ UndefinedVariable (Var "x"))
