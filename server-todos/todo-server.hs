import Haste.App
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

buildTodoEls :: MonadIO m => TodoUI -> [String] -> m ()
buildTodoEls ui todos = do
  els <- mapM createTodoEl todos
  setChildren (todoList ui) els
  where
    createTodoEl text = do
      el  <- newElem "li"
      txt <- newTextElem text
      addChild txt el
      return el

takeValue :: MonadIO m => Elem -> m String
takeValue el = do
  new <- getProp el "value"
  setProp el "value" ""
  return new

main :: IO ()
main = do
  runApp (mkConfig "ws://localhost:24601" 24601) $ do

    -- initialize the list of todos on the server
    todos <- liftServerIO $ C.newMVar ([] :: TodoList)

    -- make our todo-adding function available to client
    -- as an API call
    addTodo <- remote $ \todo -> do
      mv <- todos
      liftIO $ do
        todos <- C.takeMVar mv
        C.putMVar mv $ todos ++ [todo]
        C.readMVar mv

    -- need to expose a way to get the todos on first load
    -- too
    getTodos <- remote $ todos >>= liftIO . C.readMVar

    -- client application
    runClient $ withElem "root" buildUI >>= \ui -> do

      -- create todo elements for everything in server's
      -- cache o' todos
      onServer getTodos >>= buildTodoEls ui

      -- event handler to add new todo to server's list
      (input ui) `onEvent` OnKeyUp $ \keycode -> case keycode of
        13 -> do
          newTodo <- takeValue (input ui)
          updated <- onServer $ addTodo <.> newTodo
          buildTodoEls ui updated
        _ -> return ()

      -- same thing, but for mouse cause i can't figure out
      -- how to share the same event-handler
      (saveButton ui) `onEvent` OnClick $ \_ _ -> do
        newTodo <- takeValue (input ui)
        updated <- onServer $ addTodo <.> newTodo
        buildTodoEls ui updated
