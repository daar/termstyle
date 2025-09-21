# TermStyle

**TermStyle** brings **colorful, styled output** to your CLI applications with the familiar **[Tailwind CSS](https://tailwindcss.com/) API**. It allows building a **beautiful and expressive terminal** with ease.

> TermStyle is inspired by [Termwind](https://github.com/nunomaduro/termwind).

<p align="center">
    <img src="img/screenshot.png" alt="Termwind example" height="300">
</p>

## Installation

Install using [Nova](https://github.com/nova-packager/nova).

```bash
nova require daar/termstyle
```

## Usage

**TermStyle** accepts valid HTML and Tailwind formatting classes.

```pascal
uses
  TermStyle;

Writeln(parse('<div class="font-bold text-red">Important!</div>'));
Writeln(parse('<div class="text-green-500 bg-gray-800">Green text on dark background</div>'));
````

### `prompt()`

The `prompt()` function may be used to prompt the user with a question.

```pascal
answer := prompt('What is your name?');
```

### Message helpers

TermStyle provides several default message helpers that display a colorful message depending on the severity.

```pascal
procedure error(const Msg: string);
procedure success(const Msg: string);
procedure warning(const Msg: string);
procedure info(const Msg: string);
```

### `banner()`

The `banner()` function may be used to show a banner on screen with a custom HTML formatting.

```pascal
banner('<span class="text-white bg-blue-400 font-bold">', '</span>', 'TERMSTYLE DEMO CLI'); 
```


## Classes Supported

All the classes supported use exactly the same logic that is available on [tailwindcss.com/docs](https://tailwindcss.com/docs).

* **[Background Color](https://tailwindcss.com/docs/background-color):** `bg-{color}-{variant}`.
* **[Text Color](https://tailwindcss.com/docs/text-color):** `text-{color}-{variant}`.
* **[Font Weight](https://tailwindcss.com/docs/font-weight#class-reference):** `font-bold`, `font-normal`.
* **[Font Style](https://tailwindcss.com/docs/font-style#italics):** `italic`.
* **[Text Decoration](https://tailwindcss.com/docs/text-decoration):** `underline`, `line-through`.


## HTML Elements Supported

All the elements have the capability to use the `class` attribute.

### `<div>`

The `<div>` element can be used as a block type element.

**Default Styles**: `block`

```pascal
render('<div>This is a div element.</div>');
```

### `<p>`

The `<p>` element can be used as a paragraph.

```pascal
render('<p>This is a paragraph.</p>');
```

### `<span>`

The `<span>` element can be used as an inline text container.

```pascal
render(
    '<p>' +
    'This is a CLI app built with <span class="text-green-300">Termwind</span>.' +
    '</p>'
  );
```

### `<a>`

The `<a>` element can be used as a hyperlink. It allows to use the `href` attribute to open the link when clicked.

**Default Styles**: `text-blue-500`

```pascal
render(
    '<p>' +
    'This is a CLI app built with Termwind. <a href="/">Click here to open</a>' +
    '</p>'
  );
```

### `<b>` and `<strong>`

The `<b>`and `<strong>` elements can be used to mark the text as **bold**.

**Default Styles**: `font-bold`

```pascal
render(
    '<p>' +
    'This is a CLI app built with <b>Termwind</b>' +
    '</p>'
  );
```

### `<i>` and `<em>`

The `<i>` and `<em>` elements can be used to mark the text as *italic*.

**Default Styles**: `italic`

```pascal
render(
    '<p>' +
    'This is a CLI app built with <i>Termwind</i>.' +
    '</p>'
  );
```

### `<s>`

The `<s>`  element can be used to add a ~~line through~~ the text.

**Default Styles**: `line-through`

```pascal
render(
    '<p>' +
    'This is a CLI app built with <s>Termwind</s>.' +
    '</p>'
  );
```
