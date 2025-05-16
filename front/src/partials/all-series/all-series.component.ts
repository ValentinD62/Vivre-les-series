import {customElement, state} from "lit/decorators.js";
import {css, html, LitElement, unsafeCSS} from "lit";
import AllSeriesCSS from "./all-series.scss?inline";
import "../../components/series-card/series-card.component.ts";
import "../../components/more-information/more-information.component.ts";

//Composant pour la page de la liste de toutes les séries
@customElement('all-series')
export class AllSeriesComponent extends LitElement {

  @state()
  idDescription: number = -1;

  private showDescription(event: CustomEvent) {
    this.idDescription = event.detail;
  }

  private notShowDescription(){
    this.idDescription = -1;
  }

  render() {
    console.log("showDescription", this.idDescription);
    return html`
            <div class="all-series-container">
                <h1>Liste des séries</h1>
                <div class="all-series-container__grid">
                    <series-card-component series-id="8" @showDescriptionVisible=${this.showDescription}></series-card-component>
                    <series-card-component series-id="9" @showDescriptionVisible=${this.showDescription}></series-card-component>
                    <series-card-component series-id="10" @showDescriptionVisible=${this.showDescription}></series-card-component>
                    <series-card-component series-id="11" @showDescriptionVisible=${this.showDescription}></series-card-component>
                    <series-card-component series-id="12" @showDescriptionVisible=${this.showDescription}></series-card-component>
                    <series-card-component series-id="13" @showDescriptionVisible=${this.showDescription}></series-card-component>
                    <series-card-component series-id="14" @showDescriptionVisible=${this.showDescription}></series-card-component>
                </div>
            </div>
            ${this.idDescription !== -1 ? html`<more-information-component id-series="${this.idDescription}" @notDisplayMoreInformation=${this.notShowDescription} ></more-information-component>` : ""}
        `;
  }

  static styles = css`${unsafeCSS(AllSeriesCSS)}`;
}

declare global {
  interface HTMLElementTagNameMap {
    'all-series-component': AllSeriesComponent
  }
}
