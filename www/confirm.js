$(document).ready(function() {
  var upload = false;
  $("#help").on("click", async function(event) {
    
      Swal.fire({
        title:"Select a site and date range!",
        text:"then a list of species that have been seen here will appear ranked by most captures to least...",
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