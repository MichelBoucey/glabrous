{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import           Data.Either.Combinators (fromRight')
import qualified Data.HashMap.Strict     as H
import           Data.Maybe
import           Data.Text
import           System.Directory        (removeFile)
import           Test.Hspec
import           Text.Glabrous
import           Text.Glabrous.Types

templateText :: Text
templateText = "Affirmanti incumbit probatio. Lorem {{ipsum}} dolor sit amet, consectetur adipiscing elit. Proin bibendum mauris vitae dui venenatis pretium. Maecenas vestibulum justo at accumsan mattis. Nulla id finibus sem. Cras dolor nunc, consectetur in tincidunt id, facilisis in lorem. Nullam sed eros venenatis, tempor felis in, ultrices enim. Donec sit amet ligula ac orci cursus finibus ut eget lectus. Praesent feugiat massa non mi venenatis faucibus. Proin sollicitudin ultrices sapien, eu venenatis purus sollicitudin vel. Donec maximus id ligula in {congue}. Ut neque erat, dictum vehicula justo vel, dapibus facilisis est. Proin elementum finibus magna. Maecenas varius ultrices arcu, et auctor lectus egestas nec. Nulla facilisi. Donec eros urna, mattis viverra dapibus et, aliquet vel {{tortor}}. Etiam rutrum lacinia turpis, vitae faucibus nibh pharetra gravida.\n\nVivamus venenatis nec sapien eu rutrum. Mauris {{lectus}} tortor, suscipit eget tristique in, finibus a augue. Nulla euismod fermentum ligula, eget viverra eros porttitor eget. Integer lacinia id libero vitae laoreet. Vivamus sit amet molestie mi, in posuere diam. Nulla ornare luctus diam, at sodales neque porta ac. Nunc mi lectus, imperdiet in lorem pretium, volutpat {{vehicula}} sem. Aliquam quis orci eu nisl consequat bibendum. Fusce efficitur {{bibendum}} tristique. Integer lacinia turpis sit amet neque imperdiet tempor. Aenean {dolor}} odio, congue sed lorem nec, {{condimentum}} tempor lorem. Ut maximus lorem in augue tincidunt, nec dapibus erat luctus. Vivamus varius vitae lacus sed elementum. Nulla feugiat porttitor dictum. Donec posuere nisl at tincidunt gravida. Fusce semper maximus metus sit amet tempus. Quisque tempus neque ligula, efficitur molestie quam convallis vel. Donec porttitor rutrum {{tellus}}, vel faucibus felis dapibus at. Mauris id tortor {{faucibus}}, tempus velit sit amet, pretium tellus. Aliquam luctus mauris dui,{{cursus}} volutpat quam egestas vel. In vel neque eu eros efficitur aliquam. Donec efficitur tortor massa. Praesent molestie enim justo, ac gravida enim tristique nec. In non rutrum mi. Morbi viverra, mi vestibulum gravida iaculis, nulla ipsum fringilla nisl, at vestibulum nisl neque at est. Proin viverra varius nisi, quis tincidunt odio suscipit nec. Vestibulum nisi justo, fermentum {{commodo}} accumsan id, dictum dictum felis. Fusce rutrum suscipit urna in ornare. Duis volutpat arcu et rutrum pulvinar. Vestibulum mattis scelerisque ligula, ut fermentum ligula. Fusce ut semper ipsum, at {{vestibulum}} felis. Ut semper turpis sit amet diam commodo bibendum. Ut vulputate quam nec massa pretium, vitae sodales quam ultrices. Fusce imperdiet mi sed maximus {{lobortis}}. Donec nulla quam, luctus a placerat vel, scelerisque et lacus. Nulla sagittis ipsum ac viverra eleifend. Praesent et sem ut erat ornare rutrum. Praesent ullamcorper dolor nunc, nec aliquam massa varius sed.\n\nUt luctus ante ut venenatis blandit. Aenean cursus leo non finibus cursus. Suspendisse sed enim hendrerit neque {{fermentum}} rutrum eu ut nulla. Curabitur consequat orci quam, et condimentum lacus rutrum interdum. In in {{maximus}} eros. Duis rhoncus nisi at volutpat aliquam. Quisque vestibulum {{venenatis}} nisi, sed {{sagittis}} est ultrices ut. Morbi quis ipsum {{vitae}} erat mollis egestas nec viverra diam. Aenean imperdiet vestibulum risus vel bibendum. Aenean eget augue luctus, congue erat vel, hendrerit nunc. Ut metus risus, viverra non varius in, {{rhoncus}} sed. Fabricando fit faber."

variablesList :: [(Text,Text)]
variablesList = [("ipsum","IPSUM"),("lobortis","LOBORTIS"),("condimentum","CONDIMENTUM"),("maximus","MAXIMUS"),("sagittis","SAGITTIS"),("vestibulum","VESTIBULUM"),("cursus","CURSUS"),("vehicula","VEHICULA"),("faucibus","FAUCILUS"),("vitae","VITAE"),("tellus","TELLUS"),("lectus","LECTUS")]

templateFile :: FilePath
templateFile = "./tests/template"

template' :: Template
template' = fromRight' $ fromText "Affirmanti incumbit probatio. Lorem {{dolor}} sit amet, {{consectetur}} adipiscing elit. Proin bibendum mauris {{vitae}} dui venenatis pretium. Maecenas vestibulum justo at accumsan {{mattis}}. Nulla id finibus sem. Cras dolor nunc, consectetur in tincidunt id, facilisis in lorem. Nullam sed eros venenatis, tempor felis in, ultrices enim. Donec sit amet ligula ac orci cursus finibus ut eget lectus. Praesent feugiat massa non mi venenatis {{faucibus}}. Fabricando fit faber."

context' :: Context
context' = fromList [("dolor"," Dolor! "),("consectetur"," Consectetur! "),("vitae"," Vitae! "),("mattis"," Mattis! "),("faucibus"," Faucibus! ")]

pcontext :: Context
pcontext = fromList [("dolor"," Dolor! "),("vitae"," Vitae! "),("faucibus"," Faucibus! ")]

contextFile :: FilePath
contextFile = "./tests/template.json"

clean :: IO ()
clean = do
    removeFile contextFile
    removeFile templateFile

main :: IO ()
main = hspec $ do

    describe "writeTemplateFile" $
        it "write a template to a file as it is" $
            writeTemplateFile templateFile $ fromRight' $ fromText templateText

    describe "readTemplateFile" $
        it "loads a template from a file" $ do
            readTemplateFile "./tests/template" `shouldReturn` Right (Template {content = [Literal "Affirmanti incumbit probatio. Lorem ",Tag "ipsum",Literal " dolor sit amet, consectetur adipiscing elit. Proin bibendum mauris vitae dui venenatis pretium. Maecenas vestibulum justo at accumsan mattis. Nulla id finibus sem. Cras dolor nunc, consectetur in tincidunt id, facilisis in lorem. Nullam sed eros venenatis, tempor felis in, ultrices enim. Donec sit amet ligula ac orci cursus finibus ut eget lectus. Praesent feugiat massa non mi venenatis faucibus. Proin sollicitudin ultrices sapien, eu venenatis purus sollicitudin vel. Donec maximus id ligula in ",Literal "{",Literal "congue",Literal "}",Literal ". Ut neque erat, dictum vehicula justo vel, dapibus facilisis est. Proin elementum finibus magna. Maecenas varius ultrices arcu, et auctor lectus egestas nec. Nulla facilisi. Donec eros urna, mattis viverra dapibus et, aliquet vel ",Tag "tortor",Literal ". Etiam rutrum lacinia turpis, vitae faucibus nibh pharetra gravida.\n\nVivamus venenatis nec sapien eu rutrum. Mauris ",Tag "lectus",Literal " tortor, suscipit eget tristique in, finibus a augue. Nulla euismod fermentum ligula, eget viverra eros porttitor eget. Integer lacinia id libero vitae laoreet. Vivamus sit amet molestie mi, in posuere diam. Nulla ornare luctus diam, at sodales neque porta ac. Nunc mi lectus, imperdiet in lorem pretium, volutpat ",Tag "vehicula",Literal " sem. Aliquam quis orci eu nisl consequat bibendum. Fusce efficitur ",Tag "bibendum",Literal " tristique. Integer lacinia turpis sit amet neque imperdiet tempor. Aenean ",Literal "{",Literal "dolor",Literal "}}",Literal " odio, congue sed lorem nec, ",Tag "condimentum",Literal " tempor lorem. Ut maximus lorem in augue tincidunt, nec dapibus erat luctus. Vivamus varius vitae lacus sed elementum. Nulla feugiat porttitor dictum. Donec posuere nisl at tincidunt gravida. Fusce semper maximus metus sit amet tempus. Quisque tempus neque ligula, efficitur molestie quam convallis vel. Donec porttitor rutrum ",Tag "tellus",Literal ", vel faucibus felis dapibus at. Mauris id tortor ",Tag "faucibus",Literal ", tempus velit sit amet, pretium tellus. Aliquam luctus mauris dui,",Tag "cursus",Literal " volutpat quam egestas vel. In vel neque eu eros efficitur aliquam. Donec efficitur tortor massa. Praesent molestie enim justo, ac gravida enim tristique nec. In non rutrum mi. Morbi viverra, mi vestibulum gravida iaculis, nulla ipsum fringilla nisl, at vestibulum nisl neque at est. Proin viverra varius nisi, quis tincidunt odio suscipit nec. Vestibulum nisi justo, fermentum ",Tag "commodo",Literal " accumsan id, dictum dictum felis. Fusce rutrum suscipit urna in ornare. Duis volutpat arcu et rutrum pulvinar. Vestibulum mattis scelerisque ligula, ut fermentum ligula. Fusce ut semper ipsum, at ",Tag "vestibulum",Literal " felis. Ut semper turpis sit amet diam commodo bibendum. Ut vulputate quam nec massa pretium, vitae sodales quam ultrices. Fusce imperdiet mi sed maximus ",Tag "lobortis",Literal ". Donec nulla quam, luctus a placerat vel, scelerisque et lacus. Nulla sagittis ipsum ac viverra eleifend. Praesent et sem ut erat ornare rutrum. Praesent ullamcorper dolor nunc, nec aliquam massa varius sed.\n\nUt luctus ante ut venenatis blandit. Aenean cursus leo non finibus cursus. Suspendisse sed enim hendrerit neque ",Tag "fermentum",Literal " rutrum eu ut nulla. Curabitur consequat orci quam, et condimentum lacus rutrum interdum. In in ",Tag "maximus",Literal " eros. Duis rhoncus nisi at volutpat aliquam. Quisque vestibulum ",Tag "venenatis",Literal " nisi, sed ",Tag "sagittis",Literal " est ultrices ut. Morbi quis ipsum ",Tag "vitae",Literal " erat mollis egestas nec viverra diam. Aenean imperdiet vestibulum risus vel bibendum. Aenean eget augue luctus, congue erat vel, hendrerit nunc. Ut metus risus, viverra non varius in, ",Tag "rhoncus",Literal " sed. Fabricando fit faber."]})

    describe "tagsOf" $
        it "returns the list of tags in a template" $
            tagsOf <$> fromRight' <$> readTemplateFile templateFile `shouldReturn` ["ipsum","tortor","lectus","vehicula","bibendum","condimentum","tellus","faucibus","cursus","commodo","vestibulum","lobortis","fermentum","maximus","venenatis","sagittis","vitae","rhoncus"]

    describe "fromTemplate" $
        it "builds a Context from a template" $
            fromTemplate <$> fromRight' <$> readTemplateFile templateFile `shouldReturn` Context {variables = H.fromList [("ipsum",""),("lobortis",""),("condimentum",""),("maximus",""),("sagittis",""),("vestibulum",""),("cursus",""),("vehicula",""),("faucibus",""),("vitae",""),("tellus",""),("lectus",""),("tortor",""),("commodo",""),("fermentum",""),("ipsum",""),("rhoncus",""),("bibendum",""),("venenatis","")]}

    describe "setVariables" $
        it "populates and/or updates a Context" $
            setVariables variablesList <$> fromTemplate <$> fromRight' <$> readTemplateFile templateFile `shouldReturn` Context {variables = H.fromList [("ipsum","IPSUM"),("lobortis","LOBORTIS"),("condimentum","CONDIMENTUM"),("maximus","MAXIMUS"),("sagittis","SAGITTIS"),("vestibulum","VESTIBULUM"),("cursus","CURSUS"),("vehicula","VEHICULA"),("faucibus","FAUCILUS"),("vitae","VITAE"),("tellus","TELLUS"),("lectus","LECTUS"),("tortor",""),("commodo",""),("fermentum",""),("rhoncus",""),("bibendum",""),("venenatis","")]}

    describe "initContextFile" $
        it "creates an empty of value context file" $
            fromTemplate <$> fromRight' <$> readTemplateFile templateFile >>= initContextFile contextFile

    describe "readContextFile" $
        it "gets a context from a file" $
            readContextFile contextFile `shouldReturn` Just (Context {variables = H.fromList [("lobortis",""),("condimentum",""),("maximus",""),("sagittis",""),("vestibulum",""),("cursus",""),("vehicula",""),("faucibus",""),("vitae",""),("tellus",""),("lectus",""),("tortor",""),("commodo",""),("fermentum",""),("ipsum",""),("rhoncus",""),("bibendum",""),("venenatis","")]})

    describe "unsetContext" $
        it "gets a Context made of unset variables of the given context" $
        fromJust <$> unsetContext <$> setVariables variablesList <$> fromTemplate <$> fromRight' <$> readTemplateFile templateFile `shouldReturn` Context {variables = H.fromList [("tortor",""),("commodo",""),("fermentum",""),("rhoncus",""),("bibendum",""),("venenatis","")]}
 
    describe "isSet" $
        it "true if all variables are not empty" $
        (return $ isSet context') `shouldReturn` True
 
    describe "writeContextFile" $
        it "writes a context to a file" $ do
            !acontext <- fromJust <$> readContextFile contextFile
            writeContextFile contextFile acontext

    describe "process" $
        it "apply to a template a context to output a text" $
            (return $ process template' context') `shouldReturn` "Affirmanti incumbit probatio. Lorem  Dolor!  sit amet,  Consectetur!  adipiscing elit. Proin bibendum mauris  Vitae!  dui venenatis pretium. Maecenas vestibulum justo at accumsan  Mattis! . Nulla id finibus sem. Cras dolor nunc, consectetur in tincidunt id, facilisis in lorem. Nullam sed eros venenatis, tempor felis in, ultrices enim. Donec sit amet ligula ac orci cursus finibus ut eget lectus. Praesent feugiat massa non mi venenatis  Faucibus! . Fabricando fit faber."

    describe "partialProcess" $
        it "apply to a template a context to output a template" $
            (return $ partialProcess template' pcontext) `shouldReturn` Template {content = [Literal "Affirmanti incumbit probatio. Lorem ",Literal " Dolor! ",Literal " sit amet, ",Tag "consectetur",Literal " adipiscing elit. Proin bibendum mauris ",Literal " Vitae! ",Literal " dui venenatis pretium. Maecenas vestibulum justo at accumsan ",Tag "mattis",Literal ". Nulla id finibus sem. Cras dolor nunc, consectetur in tincidunt id, facilisis in lorem. Nullam sed eros venenatis, tempor felis in, ultrices enim. Donec sit amet ligula ac orci cursus finibus ut eget lectus. Praesent feugiat massa non mi venenatis ",Literal " Faucibus! ",Literal ". Fabricando fit faber."]}

    describe "partialProcess'" $
        it "apply to a template with a subcontext outputs a template with its tags" $
            (return $ partialProcess' template' pcontext) `shouldReturn` Partial {template = Template {content = [Literal "Affirmanti incumbit probatio. Lorem ",Literal " Dolor! ",Literal " sit amet, ",Tag "consectetur",Literal " adipiscing elit. Proin bibendum mauris ",Literal " Vitae! ",Literal " dui venenatis pretium. Maecenas vestibulum justo at accumsan ",Tag "mattis",Literal ". Nulla id finibus sem. Cras dolor nunc, consectetur in tincidunt id, facilisis in lorem. Nullam sed eros venenatis, tempor felis in, ultrices enim. Donec sit amet ligula ac orci cursus finibus ut eget lectus. Praesent feugiat massa non mi venenatis ",Literal " Faucibus! ",Literal ". Fabricando fit faber."]}, tags = ["consectetur","mattis"]}

    describe "compress" $
        it "optimize a template after partialProcess(')" $
            (return $ compress $ partialProcess template' pcontext) `shouldReturn` Template {content = [Literal "Affirmanti incumbit probatio. Lorem  Dolor!  sit amet, ",Tag "consectetur",Literal " adipiscing elit. Proin bibendum mauris  Vitae!  dui venenatis pretium. Maecenas vestibulum justo at accumsan ",Tag "mattis",Literal ". Nulla id finibus sem. Cras dolor nunc, consectetur in tincidunt id, facilisis in lorem. Nullam sed eros venenatis, tempor felis in, ultrices enim. Donec sit amet ligula ac orci cursus finibus ut eget lectus. Praesent feugiat massa non mi venenatis  Faucibus! . Fabricando fit faber."]}

    describe "tagsRename" $
        it "renames tags for a new template" $
            (return $ tagsRename [("consectetur","causatur"),("mattis","generis")] $ compress $ partialProcess template' pcontext) `shouldReturn` Template {content = [Literal "Affirmanti incumbit probatio. Lorem  Dolor!  sit amet, ",Tag "causatur",Literal " adipiscing elit. Proin bibendum mauris  Vitae!  dui venenatis pretium. Maecenas vestibulum justo at accumsan ",Tag "generis",Literal ". Nulla id finibus sem. Cras dolor nunc, consectetur in tincidunt id, facilisis in lorem. Nullam sed eros venenatis, tempor felis in, ultrices enim. Donec sit amet ligula ac orci cursus finibus ut eget lectus. Praesent feugiat massa non mi venenatis  Faucibus! . Fabricando fit faber."]}

    describe "partialProcess'" $ afterAll_ clean $
        it "apply to a template with a context outputs a final text" $
            (return $ partialProcess' template' context') `shouldReturn` Final "Affirmanti incumbit probatio. Lorem  Dolor!  sit amet,  Consectetur!  adipiscing elit. Proin bibendum mauris  Vitae!  dui venenatis pretium. Maecenas vestibulum justo at accumsan  Mattis! . Nulla id finibus sem. Cras dolor nunc, consectetur in tincidunt id, facilisis in lorem. Nullam sed eros venenatis, tempor felis in, ultrices enim. Donec sit amet ligula ac orci cursus finibus ut eget lectus. Praesent feugiat massa non mi venenatis  Faucibus! . Fabricando fit faber."

