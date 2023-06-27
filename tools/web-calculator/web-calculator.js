window.addEventListener("DOMContentLoaded", () => {
  let store = {
    input: ""
  }
  let imports = {
    host: {
      "get-input-size": () => { return store.input.length; }
    }
  }
  fetch("calc.wasm", {cache: "no-cache"})
  .then(response => WebAssembly.instantiateStreaming(response, imports))
  .then(res => {
    gres = res
    let instance = res.instance

    document.getElementById("form").addEventListener("submit", e => {
      e.preventDefault()
      let fd = new FormData(e.target, e.submitter)
      let input = fd.get('input')
      e.target.reset()

      store.input = input

      let result = instance.exports['handle-event']()

      let div = document.createElement("div")
      div.appendChild(new Text("Result: " + result))
      document.body.appendChild(div)
    })
  }).catch(error => {
    document.body.appendChild(new Text(error))
  })
})
