import {customElement} from "lit/decorators.js";
import {css, html, LitElement, unsafeCSS} from "lit";
import {HeaderComponent} from "../header/header.component.ts";
import BodyCSS from "./body.scss?inline";

@customElement('body-component')
export class BodyComponent extends LitElement {

    render() {
        return html`
            <div class="body-container">
                <div class="body-container__div-1">
                    <h1>My title</h1>
                    <p>My paragraph</p>
                </div>
                <div class="body-container__div-2">
                    <h2>My title 2</h2>
                    <p>My paragraph 2</p>
                </div>
            </div>
        `;
    }
    static styles = css`${unsafeCSS(BodyCSS)}`;
}

declare global {
    interface HTMLElementTagNameMap {
        'header-component': HeaderComponent
    }
}
