import {customElement} from "lit/decorators.js";
import {html, css, LitElement, unsafeCSS } from "lit";
import HeaderCSS from "./header.scss?inline";
import "../components/search-bar/search-bar.component";

@customElement('header-component')
export class HeaderComponent extends LitElement {
    render() {
        return html`
            <header class="header-container">
                <div class="header-container__div-logo">
                    <a href="/"><img src="logo_vivre_series.png" class="header-container__logo" alt="logo"></a>
                </div>
                <search-bar></search-bar>
                <div class="header-container__div-user">
                    <img src="icons_user.png" class="header-container__user" alt="logo"></a>
                </div>
            </header>
        `;
    }
    static styles = css`${unsafeCSS(HeaderCSS)}`;
}

declare global {
    interface HTMLElementTagNameMap {
        'header-component': HeaderComponent
    }
}
