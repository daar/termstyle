unit TermStyle;

{$mode objfpc}{$H+}

interface

function render(const S: string): string;

// Helper printing functions
procedure error(const Msg: string; const AClasses: string = 'bg-red-700 text-red-100 font-bold');
procedure success(const Msg: string; const AClasses: string = 'bg-green-700 text-green-100 font-bold');
procedure warning(const Msg: string; const AClasses: string = 'bg-yellow-300 text-yellow-700 font-bold');
procedure info(const Msg: string; const AClasses: string = 'bg-sky-700 text-sky-100 font-bold');
procedure banner(const Msg: string; const AClasses: string = 'bg-sky-700 text-sky-100 font-bold');

function prompt(const Msg, ADefault, AClasses: string; const SkipChar: string = ''): string;

// Diff output functions
procedure diff_header(const FileName: string);
procedure diff_hunk(const Range: string);
procedure diff_add(const LineNum: integer; const Text: string);
procedure diff_del(const LineNum: integer; const Text: string);
procedure diff_context(const LineNum: integer; const Text: string);
procedure diff_file(const UnifiedDiff: string);

implementation

uses
  Classes,
  DOM_HTML,
  SAX_HTML,
  SysUtils,
  TermStyle.Nodes;

function render(const S: string): string;
var
  Doc:    THTMLDocument;
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
  TotalWidth, Padding, i:  integer;
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

function prompt(const Msg, ADefault, AClasses: string;
  const SkipChar: string = ''): string;
begin
  if SkipChar <> '' then
    write(render(Format('<span class="%s">%s</span> <span class="text-gray-400">[%s, %s to skip]</span> : ', [AClasses, Msg, ADefault, SkipChar])))
  else if ADefault <> '' then
    write(render(Format('<span class="%s">%s</span> <span class="text-gray-400">[%s]</span> : ', [AClasses, Msg, ADefault])))
  else
    write(render(Format('<span class="%s">%s</span> : ', [AClasses, Msg])));

  ReadLn(Result);
  Result := Trim(Result);

  // Handle skip
  if (SkipChar <> '') and SameText(Result, SkipChar) then
    Result := '' // skip
  else if (Result = '') and (ADefault <> '') then
    Result := ADefault; // use default
end;

{ === Diff Functions === }

procedure diff_header(const FileName: string);
begin
  writeln(render('<span class="bg-gray-200 text-gray-700 font-bold"> ' + FileName + ' </span>'));
end;

procedure diff_hunk(const Range: string);
begin
  writeln(render('<span class="text-cyan-500">' + Range + '</span>'));
end;

procedure diff_add(const LineNum: integer; const Text: string);
var
  LineNumStr: string;
begin
  LineNumStr := Format('%4d', [LineNum]);
  writeln(render(
    '<span class="text-gray-500">' + LineNumStr + '</span>' +
    '<span class="bg-green-100 text-green-700"> + ' + Text + '</span>'
  ));
end;

procedure diff_del(const LineNum: integer; const Text: string);
var
  LineNumStr: string;
begin
  LineNumStr := Format('%4d', [LineNum]);
  writeln(render(
    '<span class="text-gray-500">' + LineNumStr + '</span>' +
    '<span class="bg-red-100 text-red-700"> - ' + Text + '</span>'
  ));
end;

procedure diff_context(const LineNum: integer; const Text: string);
var
  LineNumStr: string;
begin
  LineNumStr := Format('%4d', [LineNum]);
  writeln(render(
    '<span class="text-gray-500">' + LineNumStr + '</span>' +
    '<span class="text-gray-600">   ' + Text + '</span>'
  ));
end;

procedure diff_file(const UnifiedDiff: string);
var
  Lines: TStringList;
  i, OldLine, NewLine: integer;
  Line: string;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := UnifiedDiff;
    OldLine := 0;
    NewLine := 0;

    for i := 0 to Lines.Count - 1 do
    begin
      Line := Lines[i];

      if Length(Line) = 0 then
        continue;

      // File headers
      if (Copy(Line, 1, 4) = 'diff') or
         (Copy(Line, 1, 3) = '---') or
         (Copy(Line, 1, 3) = '+++') then
      begin
        diff_header(Line);
      end
      // Hunk header
      else if Copy(Line, 1, 2) = '@@' then
      begin
        diff_hunk(Line);
        // Parse line numbers from @@ -old,count +new,count @@
        // Simple extraction - find the +number
        OldLine := 1;
        NewLine := 1;
      end
      // Added line
      else if Line[1] = '+' then
      begin
        Inc(NewLine);
        diff_add(NewLine, Copy(Line, 2, Length(Line) - 1));
      end
      // Deleted line
      else if Line[1] = '-' then
      begin
        Inc(OldLine);
        diff_del(OldLine, Copy(Line, 2, Length(Line) - 1));
      end
      // Context line
      else if Line[1] = ' ' then
      begin
        Inc(OldLine);
        Inc(NewLine);
        diff_context(NewLine, Copy(Line, 2, Length(Line) - 1));
      end
      else
      begin
        // Other lines (like "\ No newline at end of file")
        writeln(render('<span class="text-gray-400">' + Line + '</span>'));
      end;
    end;
  finally
    Lines.Free;
  end;
end;

end.
