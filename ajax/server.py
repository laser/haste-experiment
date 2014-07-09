#!/usr/bin/env python

from bottle import run, get, request, static_file
from functools import wraps
import sys

@get('/api')
def api():
  return { 'message' : 'Hello, Moto' }

@get('/')
def index():
  return static_file('index.html', './')

@get('/ajax.js')
def ajax():
  return static_file('ajax.js', './')

run(host='localhost', port=3000)
