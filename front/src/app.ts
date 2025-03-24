import "./assets/variable.scss";
import "./my-element.ts"
import "./partials/header/header.component.ts"
import "./partials/body/body.component.ts"
import "./components/user/user.component.ts"
import { User } from "./type_interfaces/type.interfaces.ts"
import {customElement, state} from "lit/decorators.js";
import {css, html, LitElement, unsafeCSS} from "lit";
import AppCSS from "./app.scss?inline";

@customElement('app-component')
export class AppComponent extends LitElement {
    @state()
    isVisibleUserPopUp = false;

    async getUsers(): Promise<User[]> {
        const headers: Headers = new Headers()
        // Add a few headers
        headers.set('Content-Type', 'application/json')
        headers.set('Accept', 'application/json')
        // Add a custom header, which we can use to check
        headers.set('X-Custom-Header', 'CustomValue')

        const request: RequestInfo = new Request("http://localhost:8080/users", {
            method: 'GET',
            headers: headers
        })
        const response = await fetch(request);
        const data = await response.json();
        return data as User[];
    }

    showUserPopUp() {
        this.isVisibleUserPopUp = !this.isVisibleUserPopUp;
    }

    render() {
        console.log(this.getUsers());
        return html`
            <div class="app-container">
                <header-component @showUserPage="${this.showUserPopUp}"></header-component>
                <user-form ?is-visible-user-pop-up="${this.isVisibleUserPopUp}"></user-form>
                <body-component></body-component>
            </div>
        `;
    }
    static styles = css`${unsafeCSS(AppCSS)}`;
}

declare global {
    interface HTMLElementTagNameMap {
        'app-component': AppComponent;
    }
}
