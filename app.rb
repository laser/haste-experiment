require 'sinatra'

set :root, File.dirname(__FILE__)
set :public_folder, 'demos'

get "/api" do
  (Time.now.to_f * 1000).to_s
end

get "/" do
  redirect 'index.html'
end
