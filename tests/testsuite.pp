program TestSuite;

{$mode objfpc}{$H+}

uses
  SysUtils,
  TapLib,
  TermStyle;

procedure test_bg_color;
begin
  assert_equal(
    'bg-blue-500',
    #27'[48;2;59;130;246mBlue bg'#27'[0m'#27'[0m'#27'[48;2;59;130;246m'#27'[0m'#27'[0m',
    parse('<span class="bg-blue-500">Blue bg</span>')
  );
end;

procedure test_text_color;
begin
  assert_equal('text-green-300',
    #$1B'[38;2;134;239;172mGreen text'#$1B'[0m'#$1B'[0m'#$1B'[38;2;134;239;172m'#$1B'[0m'#$1B'[0m',
    parse('<span class="text-green-300">Green text</span>'));
end;

procedure test_font_bold;
begin
  assert_equal('font-bold',
    #$1B'[1mBold text'#$1B'[0m'#$1B'[0m'#$1B'[1m'#$1B'[0m'#$1B'[0m',
    parse('<span class="font-bold">Bold text</span>'));
end;

procedure test_font_normal;
begin
  parse('<span class="font-normal">Normal text</span>');
  assert_equal('font-normal',
    'Normal text'#$1B'[0m'#$1B'[0m'#$1B'[0m'#$1B'[0m',
    parse('<span class="font-normal">Normal text</span>'));
end;

procedure test_font_italic;
begin
  assert_equal('italic',
    #$1B'[3mItalic text'#$1B'[0m'#$1B'[0m'#$1B'[3m'#$1B'[0m'#$1B'[0m',
    parse('<span class="italic">Italic text</span>'));
end;

procedure test_underline;
begin
  assert_equal('underline',
    #$1B'[4mUnderlined text'#$1B'[0m'#$1B'[0m'#$1B'[4m'#$1B'[0m'#$1B'[0m',
    parse('<span class="underline">Underlined text</span>'));
end;

procedure test_line_through;
begin
  assert_equal('line-through-styling',
    'Strikethrough styling'#$1B'[0m'#$1B'[0m'#$1B'[0m'#$1B'[0m',
    parse('<span class="line-though">Strikethrough styling</span>'));
end;

procedure test_uppercase;
begin
  assert_equal('uppercase',
    'UPPERCASE TEXT',
    parse('<span class="uppercase">uppercase text</span>'));
end;

procedure test_lowercase;
begin
  assert_equal('lowercase',
    'lowercase text',
    parse('<span class="lowercase">LOWERCASE TEXT</span>'));
end;

procedure test_capitalize;
begin
  assert_equal('capitalize',
    'Capitalize each word',
    parse('<span class="capitalize">capitalize each word</span>'));
end;

procedure test_snakecase;
begin
  assert_equal('snakecase',
    'snake_case_text',
    parse('<span class="snakecase">Snake Case Text</span>'));
end;

procedure test_truncate;
begin
  assert_equal('truncate',
    'This is a very lo…',
    parse('<span class="truncate">This is a very long text string</span>'));
end;

procedure test_text_alignment;
begin
  assert_equal('text-center',
    '   Centered text   ',
    parse('<div class="text-center">Centered text</div>'));
end;

procedure test_margin;
begin
  assert_equal('m-4',
    '    Text with margin',
    parse('<div class="m-4">Text with margin</div>'));
end;

procedure test_padding;
begin
  assert_equal('p-2',
    '  Text with padding  ',
    parse('<div class="p-2">Text with padding</div>'));
end;

procedure test_space;
begin
  assert_equal('space-y-2',
    'Item1'#10#10'Item2',
    parse('<div class="space-y-2"><div>Item1</div><div>Item2</div></div>'));
end;

procedure test_width;
begin
  assert_equal('w-10',
    '          |',
    parse('<div class="w-10">|</div>'));
end;

procedure test_min_width;
begin
  assert_equal('min-w-5',
    '     X',
    parse('<div class="min-w-5">X</div>'));
end;

procedure test_max_width;
begin
  assert_equal('max-w-5',
    'Hello',
    parse('<div class="max-w-5">HelloWorld</div>'));
end;

procedure test_justify;
begin
  assert_equal('justify-between',
    'Left           Right',
    parse('<div class="justify-between"><span>Left</span><span>Right</span></div>'));
end;

procedure test_invisible;
begin
  assert_equal('invisible',
    #$1B'[8mHidden'#$1B'[0m'#$1B'[0m'#$1B'[8m'#$1B'[0m'#$1B'[0m',
    parse('<span class="invisible">Hidden</span>'));
end;

procedure test_display_hidden;
begin
  assert_equal('hidden',
    '',
    parse('<div class="hidden">Not shown</div>'));
end;

procedure test_flex_1;
begin
  assert_equal('flex-1',
    '[Flex 1]',
    parse('<div class="flex-1">Flex 1</div>'));
end;

procedure test_list_styles;
begin
  assert_equal('list-disc',
    '• Item',
    parse('<ul class="list-disc"><li>Item</li></ul>'));
end;

//procedure test_content_repeat;
//begin
//  assert_equal('content-repeat',
//    '.....',
//    parse('<div class="content-repeat-[\'.\']">.</div>'));
//end;

{ Elements }
procedure test_div;
begin
  assert_equal('<div>',
    'This is a div element.'#$1B'[0m'#$1B'[0m'#$1B'[0m'#$1B'[0m',
    parse('<div>This is a div element.</div>'));
end;

procedure test_p;
begin
  assert_equal('<p>',
    'This is a paragraph.'#$1B'[0m'#$1B'[0m'#$1B'[0m'#$1B'[0m',
    parse('<p>This is a paragraph.</p>'));
end;

procedure test_span;
begin
  assert_equal('<span>',
    'This is a CLI app built with '#$1B'[0m'#$1B'[0m'#$1B'[38;2;134;239;172mTermStyle'#$1B'[0m'#$1B'[0m'#$1B'[38;2;134;239;172m'#$1B'[0m'#$1B'[0m.'#$1B'[0m'#$1B'[0m',
    parse('This is a CLI app built with <span class="text-green-300">TermStyle</span>.'));
end;

procedure test_a;
begin
  assert_equal('<a>',
    'This is a CLI app built with TermStyle. '#$1B'[0m'#$1B'[0m'#$1B'[4;38;2;59;130;246m'#$1B']8;;/'#$07'Click here to open'#$1B'[0m'#$1B']8;;'#$07#$1B'[0m',
    parse('This is a CLI app built with TermStyle. <a href="/">Click here to open</a>'));
end;

procedure test_b;
begin
  assert_equal('<b>',
  'This is a CLI app built with '#$1B'[0m'#$1B'[0m'#$1B'[1mTermStyle'#$1B'[0m'#$1B'[0m'#$1B'[1m'#$1B'[0m'#$1B'[0m.'#$1B'[0m'#$1B'[0m'#$1B'[0m'#$1B'[0m',
  parse('<p>This is a CLI app built with <b>TermStyle</b>.</p>'));
end;

procedure test_i;
begin
  assert_equal('<i>',
    'This is a CLI app built with '#$1B'[0m'#$1B'[0m'#$1B'[3mTermStyle'#$1B'[0m'#$1B'[0m'#$1B'[3m'#$1B'[0m'#$1B'[0m.'#$1B'[0m'#$1B'[0m'#$1B'[0m'#$1B'[0m',
    parse('<p>This is a CLI app built with <i>TermStyle</i>.</p>'));
end;

procedure test_s;
begin
  assert_equal('<s>',
    'This is a CLI app built with '#$1B'[0m'#$1B'[0m'#$1B'[9mTermStyle'#$1B'[0m'#$1B'[0m'#$1B'[9m'#$1B'[0m'#$1B'[0m.'#$1B'[0m'#$1B'[0m'#$1B'[0m'#$1B'[0m',
    parse('<p>This is a CLI app built with <s>TermStyle</s>.</p>'));
end;

procedure test_br;
begin
  assert_equal('<br>',
    'This is a CLI '#10'app built with TermStyle.',
    parse('<p>This is a CLI <br>app built with TermStyle.</p>'));
end;

procedure test_ul;
begin
  assert_equal('<ul>',
    '• Item 1'#10'• Item 2',
    parse('<ul><li>Item 1</li><li>Item 2</li></ul>'));
end;

procedure test_ol;
begin
  assert_equal('<ol>',
    '1. Item 1'#10'2. Item 2',
    parse('<ol><li>Item 1</li><li>Item 2</li></ol>'));
end;

procedure test_dl;
begin
  assert_equal('<dl>',
    '🍃 TermStyle'#10'    Give your CLI apps a unique look',
    parse('<dl><dt>🍃 TermStyle</dt><dd>Give your CLI apps a unique look</dd></dl>'));
end;

procedure test_hr;
begin
  assert_equal('<hr>',
    '🍃 TermStyle'#10'──────────'#10'Give your CLI apps a unique look',
    parse('<div><div>🍃 TermStyle</div><hr><p>Give your CLI apps a unique look</p></div>'));
end;

procedure test_table;
begin
  assert_equal('<table>',
    'Task   Status'#10'TermStyle   ✓ Done',
    parse('<table><thead><tr><th>Task</th><th>Status</th></tr></thead><tr><th>TermStyle</th><td>✓ Done</td></tr></table>'));
end;

procedure test_pre;
begin
  assert_equal('<pre>',
    'Text in a pre element'#10'it preserves'#10'both      spaces and'#10'line breaks',
    parse('<pre>Text in a pre element\nit preserves\nboth      spaces and\nline breaks</pre>'));
end;

procedure test_code;
begin
  assert_equal('<code>',
    '20 │ try {'#10'21 │     throw new \Exception(''Something went wrong'');'#10'22 │ } catch (\Throwable $e) {'#10'23 │     report($e);'#10'24 │ }',
    parse('<code line="22" start-line="20">try {throw new \Exception(''Something went wrong'');} catch (\Throwable $e) {report($e);} }</code>'));
end;

{ Main }
begin
  start_tests;

  start_unit('Classes');
    register_test('bg', @test_bg_color);
    register_test('text color', @test_text_color);
    register_test('font-bold', @test_font_bold);
    register_test('font-normal', @test_font_normal);
    register_test('italic', @test_font_italic);
    register_test('underline', @test_underline);
    register_test('line-through', @test_line_through);
    register_test('uppercase', @test_uppercase);
    register_test('lowercase', @test_lowercase);
    register_test('capitalize', @test_capitalize);
    register_test('snakecase', @test_snakecase);
    register_test('truncate', @test_truncate);
    //register_test('text-center', @test_text_alignment);
    register_test('margin', @test_margin);
    register_test('padding', @test_padding);
    register_test('space', @test_space);
    register_test('width', @test_width);
    register_test('min-width', @test_min_width);
    register_test('max-width', @test_max_width);
    register_test('justify', @test_justify);
    register_test('invisible', @test_invisible);
    register_test('hidden', @test_display_hidden);
    register_test('flex-1', @test_flex_1);
    register_test('list styles', @test_list_styles);
    //register_test('content repeat', @test_content_repeat);
  end_unit;

  start_unit('Elements');
    register_test('div', @test_div);
    register_test('p', @test_p);
    register_test('span', @test_span);
    register_test('a', @test_a);
    register_test('b', @test_b);
    register_test('i', @test_i);
    register_test('s', @test_s);
    register_test('br', @test_br);
    register_test('ul', @test_ul);
    register_test('ol', @test_ol);
    register_test('dl', @test_dl);
    register_test('hr', @test_hr);
    register_test('table', @test_table);
    register_test('pre', @test_pre);
    register_test('code', @test_code);
  end_unit;

  // run everything
  run_tests;

  // finalize
  end_tests;
end.
