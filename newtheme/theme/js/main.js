window.addEventListener("load", () => {
  document.body.classList.remove("preload");
});

document.addEventListener("DOMContentLoaded", () => {
  const sidebar = document.querySelector("div.sidebar");
  const overlay = document.querySelector("main .overlay");
  
  document.querySelector("main header button").addEventListener("click", () => {
    sidebar.classList.add("open");
    overlay.classList.add("open");
  });

  overlay.addEventListener("click", () => {
    sidebar.classList.remove("open");
    overlay.classList.remove("open");
  });
});
