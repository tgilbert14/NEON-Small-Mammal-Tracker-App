$(document).ready(function() {
  var upload = false;
  $("#help").on("click", async function(event) {
    
      Swal.fire({
        title:"🐾 How this works",
        html:
          "<b>1.</b> Pick a NEON site <br>" +
          "<b>2.</b> Pick a date range <br>" +
          "<b>3.</b> Hit <b>Load</b> &mdash; captures rank by individual (tagID) <br>" +
          "<b>4.</b> Pick a tagID to unlock measurements, heat maps & more",
        //width: 500,
        //padding: "4em",
        //color: "dark gray",
        //background: "#fff url(/images/cowboy3_small.png)",
        //showCancelButton: true,
        focusConfirm: false,
        confirmButtonText: `Thanks Cat!`,
        //cancelButtonText: `No thanks`,
        background: "orange",
        backdrop: `
        rgb(0, 219, 255)
        url("/images/nyan-cat.gif")
        left top
        no-repeat
          `
      });
  });
  
  $("#mode").on("click", async function(event) {
    Swal.fire({
      title:"QA/QC mode engaged",
      //width: 500,
      //padding: "4em",
      //color: "dark gray",
      //background: "#fff url(/images/cowboy3_small.png)",
      //showCancelButton: true,
      focusConfirm: false,
      confirmButtonText: `meow`,
      //cancelButtonText: `No thanks`,
      background: "orange",
      backdrop: `
      rgb(0, 219, 255)
      url("/images/nyan-cat.gif")
      left top
      no-repeat
        `
    });
  });
});