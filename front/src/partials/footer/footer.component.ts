import { customElement } from "lit/decorators.js";
import { html, css, LitElement, unsafeCSS } from "lit";
import FooterCSS from "./footer.scss?inline";
import "../../components/search-bar/search-bar.component.ts";

@customElement('footer-component')
export class FooterComponent extends LitElement {

    render() {
        return html`
            <footer class="footer-container">
                <div class="footer-container__div">
                Les séries sont fournies par <a href="https://www.themoviedb.org/" target="_blank" rel="noopener noreferrer">The Movie Database</a>
                    <img src="logo_tmdb.svg" alt="TMDB logo" class="footer-container__img">
                </div>
            </footer>
        `;
    }
    static styles = css`${unsafeCSS(FooterCSS)}`;
}

declare global {
    interface HTMLElementTagNameMap {
        'footer-component': FooterComponent
    }
}