import { customElement, property } from "lit/decorators.js";
import { html, css, LitElement, unsafeCSS } from "lit";
import HeaderCSS from "./header.scss?inline";
import "../../components/search-bar/search-bar.component.ts";

@customElement('header-component')
export class HeaderComponent extends LitElement {

    @property({type: Boolean, attribute: "connected"})
    isConnected: boolean = false;

    showUserPage() {
        const eventDetail = { userId: 123, userName: 'JohnDoe' }; // Example data
        this.dispatchEvent(new CustomEvent('showUserPage', { detail: eventDetail }));
    }

    showAllSeriesPage() {
        this.dispatchEvent(new CustomEvent('showAllSeries'));
    }

    showHomePage() {
        this.dispatchEvent(new CustomEvent('showHomePage'));
    }

    private handleUpdateSearch(event: CustomEvent) {
        this.dispatchEvent(new CustomEvent('updateSearch', {detail: event.detail.value}));
    }

    private disconnect() {
        localStorage.removeItem('username');
        localStorage.removeItem('token');
        localStorage.removeItem('userId');
        this.dispatchEvent(new CustomEvent('disconnect'));
        this.isConnected = false;
    }

    private displayUserPart() {
        if (this.isConnected) {
            return html`
                <div class="header-container__div-user" @click="${this.disconnect}">
                    <img src="deconnexion.png" class="header-container__user" alt="deconnexion"></a>
                </div>`;
        } else {
            return html`
                <div class="header-container__div-user" @click="${this.showUserPage}">
                    <img src="icons_user.png" class="header-container__user" alt="logo"></a>
                </div>`;
        }
    }

    render() {
        return html`
            <header class="header-container">
                <div class="header-container__div-logo">
                    <a href="/"><img src="logo_vivre_series.png" class="header-container__logo" alt="logo" @click=${this.showHomePage}></a>
                </div> 
                <search-bar @updateSearch=${this.handleUpdateSearch}></search-bar>
                <div class="header-container__div-series" @click="${this.showAllSeriesPage}">
                    Les Séries
                </div>
                ${this.displayUserPart()}
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
