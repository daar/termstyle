unit TermStyle;

{$mode objfpc}{$H+}

interface

function render(const S: string): string;

// Helper printing functions
procedure error(const Msg: string; const AClasses: string = 'bg-red-700 text-red-100 font-bold');
procedure success(const Msg: string; const AClasses: string = 'bg-green-700 text-green-100 font-bold');
procedure warning(const Msg: string; const AClasses: string = 'bg-yellow-700 text-yellow-100 font-bold');
procedure info(const Msg: string; const AClasses: string = 'bg-sky-700 text-sky-100 font-bold');
procedure banner(const Msg: string; const AClasses: string = 'bg-sky-700 text-sky-100 font-bold');

function prompt(const Msg: string; const AClasses: string = 'text-fuchsia-500'): string;

implementation

uses
  Classes,
  SAX_HTML,
  SysUtils,
  DOM_HTML,
  TermStyle.Nodes;

function render(const S: string): string;
var
  Doc: THTMLDocument;
  WrappedData: string;
  RootHtmlNode: THtmlBody;
  Stream: TStringStream;
begin
  Doc := THTMLDocument.Create;
  try
    // Render the HTML snippet
    WrappedData := '<body>' + S + '</body>';

    Stream := TStringStream.Create(WrappedData);
    try
      ReadHTMLFile(Doc, Stream);
    finally
      Stream.Free;
    end;

    RootHtmlNode := THtmlBody.Create;
    try
      // Traverse from the root
      TraverseNode(Doc.DocumentElement, RootHtmlNode);

      Result := RootHtmlNode.Render;
    finally
      RootHtmlNode.Free;
    end;
  finally
    Doc.Free;
  end;
end;

{ === Helper Functions === }

procedure error(const Msg: string; const AClasses: string);
begin
  writeln(render('<div class="' + AClasses + '"> ERROR </div> ' + Msg));
  writeln;
end;

procedure success(const Msg: string; const AClasses: string);
begin
  writeln(render('<div class="' + AClasses + '"> SUCCESS </div> ' + Msg));
  writeln;
end;

procedure warning(const Msg: string; const AClasses: string);
begin
  writeln(render('<div class="' + AClasses + '"> WARNING </div> ' + Msg));
  writeln;
end;

procedure info(const Msg: string; const AClasses: string);
begin
  writeln(render('<div class="' + AClasses + '"> INFO </div> ' + Msg));
  writeln;
end;

procedure banner(const Msg: string; const AClasses: string);
var
  TotalWidth, Padding, i: integer;
  Line, OpenTag, CloseTag: string;
begin
  OpenTag := '<span class="' + AClasses + '">';
  CloseTag := '</span>';

  TotalWidth := 50; // total banner width
  if Length(Msg) >= TotalWidth then
    Padding := 0
  else
    Padding := (TotalWidth - Length(Msg)) div 2;

  // Top line (spaces with background)
  Line := '';
  for i := 1 to TotalWidth do
    Line := Line + ' ';
  writeln(render(OpenTag + Line + CloseTag));

  // Msg line, centered
  Line := '';
  for i := 1 to Padding do
    Line := Line + ' ';
  Line := Line + Msg;
  while Length(Line) < TotalWidth do
    Line := Line + ' ';
  writeln(render(OpenTag + Line + CloseTag));

  // Bottom line (spaces with background)
  Line := '';
  for i := 1 to TotalWidth do
    Line := Line + ' ';
  writeln(render(OpenTag + Line + CloseTag));
end;


function prompt(const Msg: string; const AClasses: string): string;
begin
  write(render('<div class="' + AClasses + '">' + Msg + '></div> '));
  ReadLn(Result);
end;

end.
