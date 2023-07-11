window.addEventListener("DOMContentLoaded", () => {
  let imports = {
    host: {
      "length": (arg) => { return arg.length; }
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

      let result = instance.exports['handle-event'](input)

      let div = document.createElement("div")
      div.appendChild(new Text("Result: " + result))
      document.body.appendChild(div)
    })
  }).catch(error => {
    document.body.appendChild(new Text(error))
  })
})
