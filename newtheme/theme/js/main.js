window.addEventListener("load", () => {
  document.body.classList.remove("preload");
});

document.addEventListener("DOMContentLoaded", () => {
  const nav = document.querySelector("div.menu > nav");
  
  document.querySelector("#btnNav").addEventListener("click", () => {
    nav.classList.add("open");
  });

  document.querySelector("div.menu nav .overlay").addEventListener("click", () => {
    nav.classList.remove("open");
  });
});
