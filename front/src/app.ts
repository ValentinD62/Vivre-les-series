import "./assets/variable.scss";
import "./my-element.ts"
import "./partials/header/header.component.ts"
import {customElement} from "lit/decorators.js";
import { html, LitElement } from "lit";

@customElement('app-component')
export class AppComponent extends LitElement {
    render() {
        return html`
            <header-component></header-component>
            <my-element>
                <h1>Vite + Lit</h1>
            </my-element>
        `;
    }
}

declare global {
    interface HTMLElementTagNameMap {
        'app-component': AppComponent
    }
}
