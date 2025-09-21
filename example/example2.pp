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
  banner('<div class="text-white bg-blue-400 font-bold">', '</div>',
    'TERMSTYLE DEMO CLI');
  writeln;

  // --- Info block ---
  info('Welcome to the TermStyle demo! This showcases colorful terminal styling.');

  // --- Success message ---
  success('Installation completed successfully!');

  // --- Warning message ---
  warning('Low disk space detected. Consider cleaning up.');

  // --- Error message ---
  error('Failed to connect to the server. Please check your network.');
  writeln;

  // --- Styled text examples ---
  writeln(render('<div class="font-bold font-underline">Bold & Underlined</div>'));
  writeln(render(
    '<div><div class="text-red-500">Red text </div><div class="text-green-500">Green text </div><div class="text-blue-500">Blue text </div></div>'));
  writeln(render(
    '<div><div class="bg-yellow-300 text-black">Black on bright yellow background</div></div>'));
  writeln(render('<div><div class="text-pink-400 italic">Italic bright magenta text</div></div>'));
  writeln(render('<div class="invert">Reversed colors text</div>'));

  // --- Simulated menu ---
  writeln('');
  writeln(render('<div class="text-cyan-400">Available Commands:</div>'));
  writeln(render('  <div class="text-green-500">install</div> Install the package'));
  writeln(render('  <div class="text-yellow-500">update</div>  Update to latest version'));
  writeln(render('  <div class="text-red-500">remove</div>  Remove the package'));
  writeln(render('  <div class="text-purple-500">help</div>    Show help'));


  // --- Prompt example ---
  writeln('');
  write(render('<div class="text-pink-400">Enter command></div> '));
  writeLn('<user input simulation>');  // just for screenshot
  writeln;

  // --- Footer ---
  banner('<div class="bg-blue-400 text-white font-bold">', '</div>', 'END OF DEMO');
end.
