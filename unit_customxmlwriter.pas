unit Unit_CustomXMLWriter;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, XMLRead, DOM;

type
    { TCustomXMLWriter }

    TCustomXMLWriter = class(TObject)
        FLines: TStringList;
        FLineBuffer: String;
        FLevel: Integer;
        constructor Create();
        procedure AddLine(Line: String);
        procedure WriteElement(Node: TDOMNode);
        procedure WriteText(Node: TDOMNode);
        procedure WriteComment(Node: TDOMNode);
        procedure WriteDocument(Node: TDOMNode);
        procedure WriteNode(Node: TDOMNode);
        function GetLines(): TStringList;
    end;

implementation


{ TCustomXMLWriter }

constructor TCustomXMLWriter.Create;
begin
    FLines := TStringList.Create;
    FLevel := 0;
end;

procedure TCustomXMLWriter.WriteElement(Node: TDOMNode);
var
    NodeName: String;
    Child: TDOMNode;
begin
    NodeName := Node.NodeName;

    FLineBuffer := Format('<%s', [NodeName]);

    Child := Node.FirstChild;
    if Child = nil then
        AddLine('/>')
    else begin
        FLineBuffer := FLineBuffer + '>';
        if FLevel = 0 then
            AddLine('');

        Inc(FLevel);
        repeat
          WriteNode(Child);
          Child := Child.NextSibling;
        until Child = nil;
        Dec(FLevel);

        AddLine(Format('</%s>', [NodeName]));

        if Node.ParentNode.LastChild <> Node then begin
            AddLine('');
            AddLine('');
        end;
    end;
end;

procedure TCustomXMLWriter.WriteText(Node: TDOMNode);
begin
    FLineBuffer := FLineBuffer + Node.NodeValue;
end;

procedure TCustomXMLWriter.WriteComment(Node: TDOMNode);
begin
    AddLine(Format('<!--%s-->', [Node.NodeValue]));
end;

procedure TCustomXMLWriter.WriteDocument(Node: TDOMNode);
var
    Child: TDOMNode;
begin
    Child := Node.FirstChild;
    while Assigned(Child) do begin
      WriteNode(Child);
      Child := Child.NextSibling;
    end;
end;

procedure TCustomXMLWriter.WriteNode(Node: TDOMNode);
begin
    case Node.NodeType of
      ELEMENT_NODE: WriteElement(Node);
      COMMENT_NODE: WriteComment(Node);
      DOCUMENT_NODE: WriteDocument(Node);
      TEXT_NODE: WriteText(node);
    end;
end;

function TCustomXMLWriter.GetLines: TStringList;
begin
    Result := FLines;
end;

procedure TCustomXMLWriter.AddLine(Line: String);
var
    Indent: String;
    i: Integer;
begin
    Indent := '';
    for i := 1 to FLevel*4 do
        Indent := Indent + ' ';

    FLines.Add(UTF8Encode(Indent + FLineBuffer + Line));

    FLineBuffer := '';
end;

end.

