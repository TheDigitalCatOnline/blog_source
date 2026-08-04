function pickRandom(arr) {
  return arr[Math.floor(Math.random() * arr.length)];
}

document.addEventListener("DOMContentLoaded", function () {
  // BANNER_IMAGES and QUOTES are injected by base.html from pelicanconf.py
  const bannerList = typeof BANNER_IMAGES === "object" && !Array.isArray(BANNER_IMAGES)
    ? Object.values(BANNER_IMAGES)
    : BANNER_IMAGES;
  if (bannerList && bannerList.length > 0) {
    const banner = document.getElementById("siteBanner");
    const attribution = document.getElementById("bannerAttribution");
    if (banner) {
      const pick = pickRandom(bannerList);
      banner.src = pick.file.startsWith("/") ? pick.file : "/" + pick.file;
      banner.alt = "Banner";
      if (attribution) {
        attribution.innerHTML = `Photo by ${pick.author} on <a href="${pick.url}">Unsplash</a>`;
      }
    }
  }

  if (typeof QUOTES !== "undefined" && QUOTES.length > 0) {
    const quoteText = document.getElementById("quoteText");
    const quoteSource = document.getElementById("quoteSource");
    if (quoteText && quoteSource) {
      const pick = pickRandom(QUOTES);
      quoteText.textContent = pick.text;
      quoteSource.textContent = pick.source;
    }
  }
});
