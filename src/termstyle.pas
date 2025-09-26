unit TermStyle;

{$mode objfpc}{$H+}

interface

function render(const S: string): string;

// Helper printing functions
procedure error(const Msg: string);
procedure success(const Msg: string);
procedure warning(const Msg: string);
procedure info(const Msg: string);
procedure banner(const Title: string; const AClasses: string = 'bg-sky-700 text-sky-100 font-bold');

function prompt(const Msg: string): string;

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

procedure error(const Msg: string);
begin
  writeln(render('<div><div class="bg-red-700 text-red-100 font-bold"> ERROR </div> ' + Msg + '</div>'));
  writeln;
end;

procedure success(const Msg: string);
begin
  writeln(render('<div><div class="bg-green-700 text-green-100 font-bold"> SUCCESS </div> ' + Msg + '</div>'));
  writeln;
end;

procedure warning(const Msg: string);
begin
  writeln(render('<div><div class="bg-yellow-700 text-yellow-100 font-bold"> WARNING </div> ' + Msg + '</div>'));
  writeln;
end;

procedure info(const Msg: string);
begin
  writeln(render('<div class="bg-sky-700 text-sky-100 font-bold"> INFO </div> ' + Msg));
  writeln;
end;

procedure banner(const Title: string; const AClasses: string);
var
  TotalWidth, Padding, i: integer;
  Line, OpenTag, CloseTag: string;
begin
  OpenTag := '<span class="' + AClasses + '">';
  CloseTag := '</span>';

  TotalWidth := 50; // total banner width
  if Length(Title) >= TotalWidth then
    Padding := 0
  else
    Padding := (TotalWidth - Length(Title)) div 2;

  // Top line (spaces with background)
  Line := '';
  for i := 1 to TotalWidth do
    Line := Line + ' ';
  writeln(render(OpenTag + Line + CloseTag));

  // Title line, centered
  Line := '';
  for i := 1 to Padding do
    Line := Line + ' ';
  Line := Line + Title;
  while Length(Line) < TotalWidth do
    Line := Line + ' ';
  writeln(render(OpenTag + Line + CloseTag));

  // Bottom line (spaces with background)
  Line := '';
  for i := 1 to TotalWidth do
    Line := Line + ' ';
  writeln(render(OpenTag + Line + CloseTag));
end;


function prompt(const Msg: string): string;
begin
  write(render('<div class="text-fuchsia-500">' + Msg + '></div> '));
  ReadLn(Result);
end;

end.
