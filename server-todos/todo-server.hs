import Control.Applicative
import Haste.App
import Data.IORef

type Todo     = String
type TodoList = [Todo]

-- our application state consists of IORef containing
-- a list of Todo-values
type AppState = IORef TodoList

data TodoUI = TodoUI {
  todoList   :: Elem,
  input      :: Elem,
  saveButton :: Elem
}

data TodoAPI = TodoAPI {
  apiAddTodo  :: Remote (Todo -> Server TodoList),
  apiGetTodos :: Remote (Server TodoList)
}

appendTodo :: Server (AppState) -> Todo -> Server TodoList
appendTodo sas todo = do
  state <- sas
  liftIO $ atomicModifyIORef state $ \todos -> (todos, todos ++ [todo])

syncTodos :: TodoUI -> Remote (Todo -> Server TodoList) -> Client ()
syncTodos ui addTodo = do
  newTodo <- takeVal $ input ui
  updated <- onServer $ addTodo <.> newTodo
  syncTodoUI ui updated

syncTodoUI :: TodoUI -> TodoList -> Client ()
syncTodoUI ui todos = do
  els <- mapM createTodoEl todos
  setChildren (todoList ui) els
  where
    createTodoEl :: String -> Client Elem
    createTodoEl text = do
      txt <- newTextElem text
      el  <- newElem "li"
      addChild txt el
      return el

buildUI :: Elem -> Client TodoUI
buildUI root = do
  [ul, input, saveButton] <- mapM newElem ["ul", "input", "button"]
  saveTxt <- newTextElem "save"
  addChild saveTxt saveButton
  setChildren root [ul, input, saveButton]
  return TodoUI { todoList=ul, input=input, saveButton=saveButton }

swapVal :: String -> Elem -> Client String
swapVal new el = do
  old <- getProp el "value"
  setProp el "value" new
  return old

takeVal :: Elem -> Client String
takeVal = swapVal ""

main :: IO ()
main = do
  runApp (mkConfig "ws://localhost:24601" 24601) $ do

    -- initialize the list of todos on the server
    todos <- liftServerIO $ newIORef ([] :: TodoList)

    -- make our todo-adding functions available to client
    -- as API calls
    f1 <- remote $ appendTodo todos
    f2 <- remote $ todos >>= liftIO . readIORef
    let api = TodoAPI f1 f2

    -- TODO: how the heck does this work??
    {-api <- TodoAPI <$> remote (appendTodo todos)-}
                   {-<*> remote (todos >>= liftIO . C.readMVar)-}

    -- client application
    runClient $ withElem "root" buildUI >>= \ui -> do

      -- create todo elements for everything in server's
      -- cache o' todos
      onServer (apiGetTodos api) >>= syncTodoUI ui

      -- event handler to add new todo to server's list
      (input ui) `onEvent` OnKeyUp $ \keycode -> case keycode of
        13 -> syncTodos ui $ apiAddTodo api
        _  -> return ()

      -- same thing, but for mouse
      (saveButton ui) `onEvent` OnClick $ const . const $ syncTodos ui $ apiAddTodo api
