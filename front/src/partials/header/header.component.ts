import {customElement} from "lit/decorators.js";
import { html, css, LitElement, unsafeCSS } from "lit";
import HeaderCSS from "./header.scss?inline";
import "../../components/search-bar/search-bar.component.ts";

@customElement('header-component')
export class HeaderComponent extends LitElement {

    showUserPage() {
        const eventDetail = { userId: 123, userName: 'JohnDoe' }; // Example data
        this.dispatchEvent(new CustomEvent('showUserPage', { detail: eventDetail }));
    }

    render() {
        return html`
            <header class="header-container">
                <div class="header-container__div-logo">
                    <a href="/"><img src="logo_vivre_series.png" class="header-container__logo" alt="logo"></a>
                </div>
                <search-bar></search-bar>
                <div class="header-container__div-user" @click="${this.showUserPage}">
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
