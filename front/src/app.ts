import "./assets/variable.scss";
import "./my-element.ts"
import "./partials/header/header.component.ts"
import "./partials/body/body.component.ts"
import "./components/user/user.component.ts"
import "./partials/all-series/all-series.component.ts"
import "./partials/search-series/search-series.component.ts"
import { User } from "./type_interfaces/type.interfaces.ts"
import { customElement, state } from "lit/decorators.js";
import { css, html, LitElement, unsafeCSS } from "lit";
import AppCSS from "./app.scss?inline";
import { getHeaders } from "./route_function.ts";

@customElement('app-component')
export class AppComponent extends LitElement {
    @state()
    isVisibleUserPopUp: boolean = false;

    @state()
    isAllSeriesVisible: boolean = false;

    @state()
    inputValue: string = "";

    @state()
    isSearchVisible: boolean = false;

    async getUsers(): Promise<User[]> {
        const header = getHeaders();

        const request: RequestInfo = new Request("http://localhost:8080/users", {
            method: 'GET',
            headers: header
        })
        const response = await fetch(request);
        const data = await response.json();
        return data as User[];
    }

    showUserPopUp() {
        this.isVisibleUserPopUp = !this.isVisibleUserPopUp;
    }

    showAllSeries() {
        this.isAllSeriesVisible = true;
        this.isSearchVisible = false;
    }

    showBody() {
        this.isAllSeriesVisible = false;
        this.isSearchVisible = false;
    }

    handleUpdateSearch(event: CustomEvent) {
        this.inputValue = event.detail;
        if (this.inputValue !== "") {
            this.isAllSeriesVisible = false;
            this.isSearchVisible = true;
        } else {
            this.isSearchVisible = false;
        }
    }

    renderPages() {
        if (this.isSearchVisible) {
            return html`<search-series input-value="${this.inputValue}"></search-series>`;
        } else if (this.isAllSeriesVisible) {
            return html`<all-series></all-series>`;
        } else {
            return html`<body-component></body-component>`;
        }
    }

    render() {
        console.log(this.getUsers());
        return html`
            <div class="app-container">
                <header-component @showUserPage=${this.showUserPopUp}
                                  @showAllSeries=${this.showAllSeries}
                                  @showHomePage=${this.showBody}
                                  @updateSearch=${this.handleUpdateSearch}
                ></header-component>
                <user-form ?is-visible-user-pop-up="${this.isVisibleUserPopUp}"></user-form>
                ${this.renderPages()}
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
