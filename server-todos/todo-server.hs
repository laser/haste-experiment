import Haste.App
import qualified Haste.App.Concurrent as H
import qualified Control.Concurrent as C

type Todo = String
type TodoList = [Todo]

data TodoUI = TodoUI { todoList :: Elem, input:: Elem, saveButton :: Elem }

buildUI :: MonadIO m => Elem -> m TodoUI
buildUI root = do
  [ul, input, saveButton] <- mapM newElem ["ul", "input", "button"]
  saveTxt <- newTextElem "save"
  addChild saveTxt saveButton
  setChildren root [ul, input, saveButton]
  return TodoUI { todoList=ul, input=input, saveButton=saveButton }

addTodo :: MonadIO m => m (C.MVar [Todo]) -> Todo -> m [Todo]
addTodo container todo = do
  current <- container
  liftIO $ do
    todos <- C.takeMVar current
    C.putMVar current $ todo : todos
    C.readMVar current

createTodoEl :: MonadIO m => String -> m Elem
createTodoEl text = do
  el <- newElem "li"
  txt <- newTextElem text
  addChild txt el
  return el

main :: IO ()
main = do
  runApp (mkConfig "ws://localhost:24601" 24601) $ do
    todos <- liftServerIO $ C.newMVar $ ([] :: TodoList)
    addTodo <- remote $ (addTodo todos)
    runClient $ withElem "root" buildUI >>= \ui ->
      (saveButton ui) `onEvent` OnClick $ \_ _ -> do
        newTodo <- getProp (input ui) "value"
        updated <- onServer $ addTodo <.> newTodo
        els     <- mapM createTodoEl $ reverse updated
        setChildren (todoList ui) els
