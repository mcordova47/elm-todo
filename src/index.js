'use strict'

require('./css/index.scss')

const Elm = require('./Main.elm')
const mountNode = document.getElementById('main')

const app = Elm.Main.embed(mountNode)

const cacheKey = 'elm-todo / todoList'

app.ports.cache.subscribe(todoList => {
  localStorage.setItem(cacheKey, todoList)
})

app.ports.retrieve.send(localStorage.getItem(cacheKey) || '[]')
