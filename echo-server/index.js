const express = require('express')
const bodyParser = require('body-parser')

const app = express()

app.use(bodyParser.json())

app.post('/api', (req, res) => {
  const start = Date.now()
  const data = req.body

  res.json({
    zap: "hello",
    version: "v1.0",
    requestTime: `${Date.now() - start}ms`
  })
})

app.listen(3000, () => {
  console.log("listening...")
})
