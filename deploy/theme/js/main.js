window.addEventListener("load", () => {
  document.body.classList.remove("preload");
});

document.addEventListener("DOMContentLoaded", () => {
  const menu = document.querySelector("div.mobile-menu");
  const button = document.querySelector("div.page header button");
  const icon = document.querySelector("div.page header button i");
  
  button.addEventListener("click", () => {
    if (icon.classList.contains("fa-bars")) {
      icon.classList.remove("fa-bars");
      icon.classList.add("fa-times");
      menu.classList.add("open");
    } else {
      icon.classList.remove("fa-times");
      icon.classList.add("fa-bars");
      menu.classList.remove("open");
    }
  });
});
