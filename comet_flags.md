# Comet Browser Experimental Flags

Welcome to the experimental features page for Comet Browser.

**Warning**: Experimental features ahead! By enabling these features, you could lose browser data or compromise your security or privacy. Enabled features apply to all users of this browser. If you are an enterprise admin you should not be using these flags in production.

Interested in cool new Chrome features? Try our beta or dev channels.

---

## Rendering and GPU

-   **Override software rendering list**: Overrides the built-in software rendering list and enables GPU-acceleration on unsupported system configurations. (`#ignore-gpu-blocklist`)
-   **Accelerated 2D canvas**: Enables the use of the GPU to perform 2d canvas rendering instead of using software rendering. (`#disable-accelerated-2d-canvas`)
-   **GPU rasterization**: Use GPU to rasterize web content. (`#enable-gpu-rasterization`)
-   **Vulkan**: Use Vulkan as the graphics backend. (`#enable-vulkan`)
-   **Skia Graphite**: Enable Skia Graphite. This will use the Dawn backend by default. (`#skia-graphite`)
-   **Zero-copy rasterizer**: Raster threads write directly to GPU memory associated with tiles. (`#enable-zero-copy`)
-   **Partial swap**: Sets partial swap behavior. (`#ui-disable-partial-swap`)
-   **Smooth Scrolling**: Animate smoothly when scrolling page content. (`#smooth-scrolling`)
-   **Overlay Scrollbars**: Enable the experimental overlay scrollbars implementation. (`#overlay-scrollbars`)
-   **Fluent Overlay scrollbars**: Stylizes scrollbars with Microsoft Fluent design and makes them overlay over the web's content. (`#fluent-overlay-scrollbars`)
-   **Fluent scrollbars**: Stylizes scrollbars with Microsoft Fluent design. (`#fluent-scrollbars`)
-   **Root scrollbar follows theme**: If enabled makes the root scrollbar follow the browser's theme color. (`#root-scrollbar-follows-browser-theme`)
-   **Force color profile**: Forces Chrome to use a specific color profile instead of the color of the window's current monitor. (`#force-color-profile`)
-   **Forced Colors**: Enables forced colors mode for web content. (`#forced-colors`)

---

## WebRTC

-   **WebRTC PQC for DTLS**: Support in WebRTC to enable PQC for DTLS. (`#webrtc-pqc-for-dtls`)
-   **Allow WebRTC to adjust the input volume**: Allow the Audio Processing Module in WebRTC to adjust the input volume during a real-time call. (`#enable-webrtc-allow-input-volume-adjustment`)
-   **Anonymize local IPs exposed by WebRTC**: Conceal local IP addresses with mDNS hostnames. (`#enable-webrtc-hide-local-ips-with-mdns`)
-   **WebRTC downmix capture audio method**: Override the method that the Audio Processing Module in WebRTC uses to downmix the captured audio to mono. (`#enable-webrtc-apm-downmix-capture-audio-method`)
-   **Enable System Audio Echo Cancellation (AEC)**: Enables usage of system AEC on Windows and Mac to prevent echo from external applications. (`#enforce-system-echo-cancellation`)
-   **Chrome-wide echo cancellation**: Run WebRTC capture audio processing in the audio process instead of the renderer processes. (`#chrome-wide-echo-cancellation`)

---

## Web Platform APIs and Features

-   **Experimental JavaScript**: Enable web pages to use experimental JavaScript features. (`#enable-javascript-harmony`)
-   **Experimental WebAssembly**: Enable web pages to use experimental WebAssembly features. (`#enable-experimental-webassembly-features`)
-   **Experimental WebAssembly JavaScript Promise Integration (JSPI)**: Enable web pages to use experimental WebAssembly JavaScript Promise Integration (JSPI) API. (`#enable-experimental-webassembly-jspi`)
-   **WebAssembly baseline compiler**: Enables WebAssembly baseline compilation and tier up. (`#enable-webassembly-baseline`)
-   **WebAssembly lazy compilation**: Enables lazy (JIT on first call) compilation of WebAssembly modules. (`#enable-webassembly-lazy-compilation`)
-   **WebAssembly tiering**: Enables tiered compilation of WebAssembly. (`#enable-webassembly-tiering`)
-   **Future V8 VM features**: This enables upcoming and experimental V8 VM features. (`#enable-future-v8-vm-features`)
-   **Experimental Web Platform features**: Enables experimental Web Platform features that are in development. (`#enable-experimental-web-platform-features`)
-   **Web HID in WebView**: Enable WebViews to access Web HID upon embedder's permission. (`#web-hid-in-web-view`)
-   **Language detection web platform API**: When enabled, JS can use the web platform's language detection API. (`#language-detection-api`)
-   **Clipboard contentsId API**: Enables the API for getting a unique token of the system clipboard's current state. (`#enable-clipboard-contents-id`)
-   **Experimental QUIC protocol**: Enable experimental QUIC protocol support. (`#enable-quic`)
-   **WebTransport Developer Mode**: Removes the requirement that all certificates used for WebTransport over HTTP/3 are issued by a known certificate root. (`#webtransport-developer-mode`)
-   **WebXR Incubations**: Enables experimental features for WebXR. (`#webxr-incubations`)
-   **Device Posture API**: Enables Device Posture API (for foldable devices). (`#device-posture`)
-   **Viewport Segments API**: Enable the viewport segment API, giving information about the logical segments of the device (dual screen and foldable devices). (`#viewport-segments`)
-   **Enables WebNN API**: Enables the Web Machine Learning Neural Network (WebNN) API. (`#web-machine-learning-neural-network`)
-   **CSS Masonry Layout**: Enable experimental CSS Masonry Layout implementation. (`#css-masonry-layout`)

---

## Security and Privacy

-   **Chrome Refresh Token Binding**: Enables binding of Chrome refresh tokens to cryptographic keys. (`#enable-chrome-refresh-token-binding`)
-   **Device Bound Session Credentials**: Enables Google session credentials binding to cryptographic keys. (`#enable-bound-session-credentials`)
-   **Local Network Access Checks**: Enables Local Network Access checks. (`#local-network-access-check`)
-   **Enable noise for canvas readbacks in Incognito**: Enable noising pixels when the contents of a canvas are read back by a script. (`#enable-canvas-noise`)
-   **Block Cross Partition Blob URL Fetching**: Blocks fetching of cross-partitioned Blob URL. (`#block-cross-partition-blob-url-fetching`)
-   **Site Isolation**: Disables site isolation (SitePerProcess, IsolateOrigins, etc). Caution: this disables important mitigations for the Spectre CPU vulnerability. (`#site-isolation-trial-opt-out`)
-   **Strict-Origin-Isolation**: Experimental security mode that strengthens the site isolation policy. (`#strict-origin-isolation`)
-   **Tracking Protection for 3PCD**: Enables the tracking protection UI + prefs that will be used for the 3PCD 1%. (`#tracking-protection-3pcd`)
-   **Deprecate the unload event**: Controls the default for Permissions-Policy unload. (`#deprecate-unload`)
-   **Enable Fingerprinting Protection Blocklist**: May block fingerprinting resources from loading in a 3p context. (`#enable-fingerprinting-protection-blocklist`, `#enable-fingerprinting-protection-blocklist-incognito`)

---

## User Interface

-   **Touch UI Layout**: Enables touch UI layout in the browser's top chrome. (`#top-chrome-touch-ui`)
-   **Task Manager Desktop Refresh**: Enables a refreshed design for the Task Manager on Desktop platforms. (`#enable-task-manager-desktop-refresh`)
-   **Revamped Delete Browsing Data dialog**: Enables a revamped Delete Browsing Data dialog on Desktop. (`#dbd-revamp-desktop`)
-   **WebUI tab strip**: When enabled makes use of a WebUI-based tab strip. (`#webui-tab-strip`)
-   **Tab Scrolling**: Enables tab strip to scroll left and right when full. (`#scrollable-tabstrip`)
-   **Side Panel Resizing**: Allows users to resize the side panel and persist the width across browser sessions. (`#side-panel-resizing`)
-   **Auto Dark Mode for Web Contents**: Automatically render all web contents using a dark theme. (`#enable-force-dark`)

---

## Extensions

-   **Extensions on chrome:// URLs**: Enables running extensions on chrome:// URLs. (`#extensions-on-chrome-urls`)
-   **Extensions on chrome-extension:// URLs**: Enables running extensions on chrome-extension:// URLs. (`#extensions-on-extension-urls`)
-   **Extensions Menu Access Control**: Enables a redesigned extensions menu that allows the user to control extensions site access. (`#extensions-menu-access-control`)

---

## Enterprise and Developers

-   **Enable enterprise profile badging on the avatar**: Enable enterprise profile badging on the toolbar avatar. (`#enable-enterprise-profile-badging-for-avatar`)
-   **Enable enterprise badging on the New Tab Page**: Enable enterprise profile badging in the footer on the New Tab Page. (`#enable-enterprise-badging-for-ntp-footer`)
-   **Collaboration Entreprise V2**: Enables the collaboration feature for entreprise users within the same domain. (`#collaboration-entreprise-v2`)
-   **DevTools Privacy UI**: Enables the Privacy UI in the current 'Security' panel in DevTools. (`#devtools-privacy-ui`)
-   **DevTools Project Settings**: DevTools will try to fetch project settings from a `com.chrome.devtools.json` file. (`#devtools-project-settings`)
-   **Use Google Payments sandbox servers**: For developers: use the sandbox service for Google Payments API calls. (`#wallet-service-use-sandbox`)

---

## Perplexity Comet Features

-   **Perplexity Backend Url**: Overrides the backend url for all browser components. Defaults to production. (`#perplexity-backend-url`)
-   **Perplexity Autoupdate Comet**: Enables auto-updating of Comet Browser and sub-components. (`#perplexity-autoupdate`)
