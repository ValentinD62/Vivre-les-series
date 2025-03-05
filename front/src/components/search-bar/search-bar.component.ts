import {customElement} from "lit/decorators.js";
import {html, css, LitElement, unsafeCSS } from "lit";
import SearchBarCSS from "./search-bar.scss?inline";

@customElement('search-bar')
export class SearchBarComponent extends LitElement {
    render() {
        return html`
            <div class="search-bar-container">
                <div class="search-bar-container__div-bar"></div>
                <div class="search-bar-container__div-icon">
                    <img src="icons_loupe.png" class="search-bar-container__img" alt="logo">
                </div>
            </div>
        `;
    }
    static styles = css`${unsafeCSS(SearchBarCSS)}`;
}

declare global {
    interface HTMLElementTagNameMap {
        'search-bar': SearchBarComponent
    }
}
