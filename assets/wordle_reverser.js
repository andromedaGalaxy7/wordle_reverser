let selected_color = 'c';
let wordle_grid = [
    /*
    * w = wrong // gray
    * s = semi-correct // yellow
    * c = correct // green
    * */
    ['w', 'w', 'w', 'w', 'w'],
    ['w', 'w', 'w', 'w', 'w'],
    ['w', 'w', 'w', 'w', 'w'],
    ['w', 'w', 'w', 'w', 'w'],
    ['w', 'w', 'w', 'w', 'w'],
    ['w', 'w', 'w', 'w', 'w']
];

/*
 Change the selected color when the color selector box is chosen
 */

function clear_selected(){
    const boxes = document.getElementsByClassName("color_choice_text");
    for(const box of boxes){
        box.classList.remove("selected");
    }
}

function to_correct(){
    selected_color = 'c';
    clear_selected();
    document.getElementById("correct_box").classList.add("selected");
}

function to_semi_correct(){
    selected_color = 's';
    clear_selected();
    document.getElementById("semi_correct_box").classList.add("selected");
}

function to_wrong(){
    selected_color = 'w';
    clear_selected();
    document.getElementById("wrong_box").classList.add("selected");
}

/*
Add event listeners to the page which
will in turn add event listeners to the letter box
 */

document.addEventListener('DOMContentLoaded', () => {
    const letterboxes = document.getElementsByClassName("letterbox");
    for(const letterbox of letterboxes){
        letterbox.addEventListener('click', (e) => {
            const trigger = e.target;
            trigger.classList.remove("c");
            trigger.classList.remove("s");
            trigger.classList.remove("w");

            // Add the current selected color
            trigger.classList.add(selected_color);
        })
    }
});

/*
function to display error in the notif_bar
 */
function display_error(msg){
    const msg_bar = document.getElementById("notif_bar");
    msg_bar.classList.remove("nodisplay");
    msg_bar.innerText = msg;
}

/*
 The solver
 */

function solve(){
    const word_input = document.getElementById("wordle_word");
    let final_word = word_input.value.toUpperCase();
    console.log(final_word);
    if(!final_word){
        display_error("Please type the final solution in the input box.");
        return;
    }
    if(final_word.length < 6){
        display_error("The final solution is too short (Not 6 letters)");
        return;
    }
    if(final_word.length > 6){
        display_error("The final solution is too long (Not 6 letters)");
        return;
    }
}