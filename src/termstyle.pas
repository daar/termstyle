unit TermStyle;

{$mode objfpc}{$H+}

interface

function parse(const S: string): string;

// Helper printing functions
procedure tsError(const Msg: string);
procedure tsSuccess(const Msg: string);
procedure tsWarn(const Msg: string);
procedure tsInfo(const Msg: string);
procedure tsBanner(const OpenTag, CloseTag, Title: string);
function Prompt(const Prefix: string): string;

implementation

uses
  Classes,
  SAX_HTML,
  SysUtils,
  DOM_HTML,
  TermStyle.Nodes;

function parse(const S: string): string;
var
  Doc: THTMLDocument;
  WrappedData: string;
  RootHtmlNode: THtmlBody;
begin
  Doc := THTMLDocument.Create;
  try
    // Parse the HTML snippet
    WrappedData := '<body>' + S + '</body>';
    ReadHTMLFile(Doc, TStringStream.Create(WrappedData));

    RootHtmlNode := THtmlBody.Create('');

    // Traverse from the root
    TraverseNode(Doc.DocumentElement, RootHtmlNode);

    Result := RootHtmlNode.Render;
  finally
    Doc.Free;
  end;
end;

{ === Helper Functions === }

procedure tsError(const Msg: string);
begin
  Writeln(parse('<div><div class="bg-red-700 text-red-100 font-bold"> ERROR </div> ' + Msg + '</div>'));
  writeln;
end;

procedure tsSuccess(const Msg: string);
begin
  Writeln(parse('<div><div class="bg-green-700 text-green-100 font-bold"> SUCCESS </div> ' + Msg + '</div>'));
  writeln;
end;

procedure tsWarn(const Msg: string);
begin
  Writeln(parse('<div><div class="bg-yellow-700 text-yellow-100 font-bold"> WARNING </div> ' + Msg + '</div>'));
  writeln;
end;

procedure tsInfo(const Msg: string);
begin
  Writeln(parse('<div class="bg-sky-700 text-sky-100 font-bold"> INFO </div> ' + Msg));
  writeln;
end;

procedure tsBanner(const OpenTag, CloseTag, Title: string);
var
  TotalWidth, Padding, i: integer;
  Line: string;
begin
  TotalWidth := 50; // total banner width
  if Length(Title) >= TotalWidth then
    Padding := 0
  else
    Padding := (TotalWidth - Length(Title)) div 2;

  // Top line (spaces with background)
  Line := '';
  for i := 1 to TotalWidth do
    Line := Line + ' ';
  Writeln(parse(OpenTag + Line + CloseTag));

  // Title line, centered
  Line := '';
  for i := 1 to Padding do
    Line := Line + ' ';
  Line := Line + Title;
  while Length(Line) < TotalWidth do
    Line := Line + ' ';
  Writeln(parse(OpenTag + Line + CloseTag));

  // Bottom line (spaces with background)
  Line := '';
  for i := 1 to TotalWidth do
    Line := Line + ' ';
  Writeln(parse(OpenTag + Line + CloseTag));
end;


function Prompt(const Prefix: string): string;
begin
  Write(parse('<div class="text-fuchsia-500">' + Prefix + '></div> '));
  ReadLn(Result);
end;

end.
