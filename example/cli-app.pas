program cli-app;

uses
  TermStyle;

begin
  writeln(render(
    '<p>' +
      'With <span class="bg-indigo-600 text-indigo-100 font-bold">TermStyle</span>, ' +
      'you can build <span class="italic">beautiful </span><span class="text-green-600">CLI</span> apps. ' +
    '</p>'
  ));
end.
