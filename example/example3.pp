program example3;

uses
  TermStyle;

begin
  writeln;
  writeln('$ ./cli-app');
  writeln;
  writeln;
  writeln(render(
    '<p>' +
      '  With <span class="bg-indigo-600 text-indigo-100 font-bold">TermStyle</span>, ' +
      'you can build <span class="italic">beautiful </span><span class="text-green-600 underline">CLI</span> apps. ' +
    '</p>'
  ));
  writeln;
  writeln;
  writeln;
end.
