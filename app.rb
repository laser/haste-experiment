require 'sinatra'

set :root, File.dirname(__FILE__)
set :public_folder, 'demos'

get "/" do
  redirect 'index.html'
end
