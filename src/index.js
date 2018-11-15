import { Main } from './Main.elm';
import {setupLocalStoragePort, setupWaterfallPort} from "./ports";
import registerServiceWorker from './registerServiceWorker';

const apiBaseUrl = "https://api.openfisca.fr/api";
const displayDisclaimer = window.localStorage.getItem("display-disclaimer")
const flags = {
    apiBaseUrl: apiBaseUrl,
    displayDisclaimer: displayDisclaimer === null ? true : JSON.parse(displayDisclaimer)
}

const app = Main.embed(document.getElementById('root'), flags);

registerServiceWorker();
if (app) { // app is null when there are compilation errors.
    setupLocalStoragePort(app)
    setupWaterfallPort(app)
}
