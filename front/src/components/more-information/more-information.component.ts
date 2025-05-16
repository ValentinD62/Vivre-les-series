import {customElement, property, state} from "lit/decorators.js";
import {css, html, LitElement, unsafeCSS} from "lit";
import MoreInformationCSS from "./more-information.scss?inline";

//Composant pour afficher les informations d'une série au clic sur l'une d'entre elles
@customElement('more-information-component')
export class MoreInformationComponent extends LitElement {

  @property({ type: Number, attribute: "id-series" })
  idSeries: number = -1;

  @state()
  loading: boolean = true;

  override connectedCallback(): void {
    super.connectedCallback();
    document.addEventListener("mousedown", this.handleOutsideClick);
  }

  override disconnectedCallback(): void {
    super.disconnectedCallback();
    document.removeEventListener("mousedown", this.handleOutsideClick);
  }

  private handleOutsideClick = (event: MouseEvent): void => {
    const target = event.target as Node;
    if (!this.contains(target)) {
      this.dispatchEvent(new CustomEvent('notDisplayMoreInformation'));
    }
  };

  render() {
    //TODO Mettre la fonction pour prendre les informations de la série en fonction de l'id
    return html`
            <div class="more-information-container">
                <div class="more-information-container__title-serie">
                    Titre de la série ${this.idSeries}
                </div>
                <div class="more-information-container__information-div">
                    <div class="more-information-container__img-div" style="background-image: url('/src/assets/test_img.png');">
                        <!--<img src="/src/assets/test_img.png" alt="nom de la serie"/>-->
                    </div>
                    <div class="more-information-container__description-div">
                        <h2>Description du film : </h2>
                        Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras ullamcorper tincidunt purus et venenatis. In ut nulla leo. Vestibulum tincidunt interdum quam, ac semper sapien lacinia ac. Cras porttitor tellus lectus, eu imperdiet tortor aliquam id. In volutpat, magna ac sagittis tincidunt, orci tortor imperdiet odio, nec malesuada lacus nunc vitae dui. Mauris fringilla, libero sit amet pretium tincidunt, libero mauris lobortis lectus, eget vehicula sapien mauris quis magna. Suspendisse pharetra, ipsum id luctus finibus, quam enim bibendum nunc, ac sollicitudin lacus libero nec eros. Donec eget lacus ac dolor rhoncus cursus eget quis metus. Duis eget rutrum neque.

                        In vitae imperdiet mauris. Duis vel turpis elementum, luctus augue id, pulvinar nunc. Nullam congue imperdiet turpis vitae pulvinar. Maecenas fermentum id tortor quis vehicula. Curabitur a varius justo. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed luctus, urna ut malesuada pretium, urna ante hendrerit tellus, at consequat libero nibh eu quam. Donec rhoncus nunc nec felis sagittis, volutpat rhoncus ipsum lobortis.

                        Aenean vestibulum libero vel quam interdum dictum id vel velit. Integer a molestie mauris. Sed facilisis mi in dolor dignissim, sit amet dictum ipsum rhoncus. Quisque iaculis ullamcorper felis ac posuere. Vestibulum fermentum sodales quam. Nulla a neque eget odio ultricies eleifend nec a eros. Morbi at tristique mauris. Cras interdum, sem ac lacinia fringilla, mi nisi elementum dui, sit amet finibus risus arcu ut quam. Mauris dictum eros ac orci accumsan venenatis id a nisl. Phasellus vitae tellus a tellus luctus posuere in quis urna. Fusce facilisis mollis nisl eu ultrices. In et justo volutpat arcu blandit imperdiet.

                        Praesent placerat enim libero, eget faucibus augue mattis eu. Integer vestibulum quam neque, consectetur mollis nisi laoreet vel. Sed tempus sed tellus sit amet mattis. Vestibulum tincidunt est nec interdum faucibus. Etiam non diam orci. Phasellus vel porta tellus. Pellentesque ut tincidunt tortor. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc condimentum malesuada mi ornare tristique. Curabitur finibus, augue eu cursus sodales, mi ante aliquet odio, id tincidunt diam nulla nec ipsum. Duis sed iaculis dolor, nec auctor quam.

                        Nam in nisi vel nulla suscipit vulputate eget congue nunc. Vivamus vestibulum velit quis lorem luctus ornare. Nunc commodo sodales tincidunt. Integer id dignissim ligula, sit amet pellentesque velit. Nunc elementum fermentum ante, vel vulputate massa faucibus ac. Mauris nisi ligula, aliquam quis viverra sit amet, varius in nulla. Mauris ac aliquet ligula. In sagittis ligula ligula, nec viverra eros sagittis scelerisque.
                    
                    </div>
                </div>
                <div class="separator"></div>
                <div class="more-information-container__footer">
                    Commentaire ?
                </div>
            </div>
        `;
  }

  static styles = css`${unsafeCSS(MoreInformationCSS)}`;
}

declare global {
  interface HTMLElementTagNameMap {
    'more-information-component': MoreInformationComponent
  }
}
