program example2;

{$mode objfpc}{$H+}

uses
  SysUtils,
  TermStyle;

begin
  if GetEnvironmentVariable('COLORTERM') = 'truecolor' then
    writeln('✅ Terminal reports TRUECOLOR support via COLORTERM')
  else
    writeln('⚠️ No COLORTERM=truecolor, need to test with a gradient.');
  writeln;

  // --- Banner ---
  tsBanner('<div class="text-white bg-blue-400 font-bold">', '</div>',
    'TERMSTYLE DEMO CLI');
  writeln;

  // --- Info block ---
  tsInfo('Welcome to the TermStyle demo! This showcases colorful terminal styling.');

  // --- Success message ---
  tsSuccess('Installation completed successfully!');

  // --- Warning message ---
  tsWarn('Low disk space detected. Consider cleaning up.');

  // --- Error message ---
  tsError('Failed to connect to the server. Please check your network.');
  writeln;

  // --- Styled text examples ---
  Writeln(parse('<div class="font-bold font-underline">Bold & Underlined</div>'));
  Writeln(parse(
    '<div><div class="text-red-500">Red text </div><div class="text-green-500">Green text </div><div class="text-blue-500">Blue text </div></div>'));
  Writeln(parse(
    '<div><div class="bg-yellow-300 text-black">Black on bright yellow background</div></div>'));
  Writeln(parse('<div><div class="text-pink-400 italic">Italic bright magenta text</div></div>'));
  Writeln(parse('<div class="invert">Reversed colors text</div>'));

  // --- Simulated menu ---
  Writeln('');
  Writeln(parse('<div class="text-cyan-400">Available Commands:</div>'));
  Writeln(parse('  <div class="text-green-500">install</div> Install the package'));
  Writeln(parse('  <div class="text-yellow-500">update</div>  Update to latest version'));
  Writeln(parse('  <div class="text-red-500">remove</div>  Remove the package'));
  Writeln(parse('  <div class="text-purple-500">help</div>    Show help'));


  // --- Prompt example ---
  Writeln('');
  Write(parse('<div class="text-pink-400">Enter command></div> '));
  WriteLn('<user input simulation>');  // just for screenshot
  writeln;

  // --- Footer ---
  tsBanner('<div class="bg-blue-400 text-white font-bold">', '</div>', 'END OF DEMO');
end.
