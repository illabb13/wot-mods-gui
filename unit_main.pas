unit Unit_Main;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin, ExtCtrls,
    XMLRead, DOM, LCLType, ComCtrls, Unit_CustomXMLWriter;

const
    XML_NAME = 'observed.xml';
    XML_MOD_PATH = 'scripts\client\mods\';
    XML_MOD_PATH_ALT = '';
    KEY_PREFIX = 'KEY_';

type

    { TFormMain }

    TFormMain = class(TForm)
        btnSave: TButton;
        cboxCellClickOption: TCheckBox;
        cboxDisabledByStartBattle: TCheckBox;
        cboxHelpMeOption: TCheckBox;
        cboxLimitationTypeOfBattles: TCheckBox;
        cboxEnabledForHT: TCheckBox;
        cboxEnabledForMT: TCheckBox;
        cboxEnabledForLT: TCheckBox;
        cboxEnabledForATSPG: TCheckBox;
        cboxEnabledForSPG: TCheckBox;
        cboxUseSquadChat: TCheckBox;
        combKey: TComboBox;
        combModKey: TComboBox;
        editEndMessage: TEdit;
        editMessage: TEdit;
        lblEnableHotKeyHelp: TLabel;
        lblUseSquadChatHelp: TLabel;
        lblEnabledForVehType: TLabel;
        lblEnabledForVehTypeHelp: TLabel;
        lblShowWhenLessInRandomDesc: TLabel;
        lblShowWhenLessDesc: TLabel;
        lblEditMessageHelp: TLabel;
        lblEndMessage: TLabel;
        lblEndMessageHelp: TLabel;
        lblEndMessageLabel: TLabel;
        lblEndMessageSec: TLabel;
        lblKey: TLabel;
        lblLimitationTypeOfBattlesHelp: TLabel;
        lblMessage: TLabel;
        lblModKey: TLabel;
        lblShowWhenLess: TLabel;
        lblShowWhenLessHelp: TLabel;
        lblTimeEndMessageHelp: TLabel;
        pcMain: TPageControl;
        spinShowWhenLessInRandom: TSpinEdit;
        spinShowWhenLess: TSpinEdit;
        spinTimeEndMessage: TSpinEdit;
        tabLimitations: TTabSheet;
        tabMessage: TTabSheet;
        tabEndMessage: TTabSheet;
        tabHotKey: TTabSheet;
        procedure btnSaveClick(Sender: TObject);
        procedure combModKeyChange(Sender: TObject);
        procedure DisabledControls(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure lblUseSquadChatHelpClick(Sender: TObject);
    private
        { private declarations }
        XmlPath: String;

        function ReadBooleanValue(Value: String): Boolean;
        function ReadIntegerValue(Value: String): Integer;
        function ReadStringValue(Value: String): String;

        procedure WriteValue(Node: TDOMNode; Value: Boolean);
        procedure WriteValue(Node: TDOMNode; Value: Integer);
        procedure WriteValue(Node: TDOMNode; Value: String);

        procedure Analyze_HotKey_Param(ParamValue: String);
        procedure Analyze_EnabledForVehType_Param(ParamValue: String);

        function Get_HotKey_Param(): String;
        function Get_EnabledForVehType_Param(): String;

        procedure ReadParamsOnTheForm(ParamName, ParamValue: String);
    public
        { public declarations }
    end;

var
    FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
var
    Doc: TXMLDocument;
    Child: TDOMNode;
    NodeName, NodeValue: String;
begin
    // init
    XmlPath := ExtractFilePath(Application.ExeName) + XML_MOD_PATH + XML_NAME;
    combModKeyChange(combModKey);
    pcMain.ActivePageIndex := 0;

    // read
    if not FileExists(XmlPath) then
        XmlPath := ExtractFilePath(Application.ExeName) + XML_MOD_PATH_ALT + XML_NAME;

    try
        ReadXMLFile(Doc, XmlPath);
    except
        Application.MessageBox(PChar('Ошибка чтения файла: ' + XML_MOD_PATH + XML_NAME), PChar(Caption), MB_ICONERROR);
        btnSave.Enabled := False;
        Exit;
	end;

    Child := Doc.DocumentElement.FirstChild;

	while Assigned(Child) do begin
	    NodeName := Child.NodeName;
	    if Child.NodeType <> COMMENT_NODE then begin
	        NodeValue := Child.FirstChild.NodeValue;
	        ReadParamsOnTheForm(NodeName, NodeValue);
	    end;
	    Child := Child.NextSibling;
	end;

    Doc.Free;
end;

procedure TFormMain.lblUseSquadChatHelpClick(Sender: TObject);
begin

end;

procedure TFormMain.ReadParamsOnTheForm(ParamName, ParamValue: String);
begin
    case ParamName of
        'disabledByStartBattle':
            cboxDisabledByStartBattle.Checked := ReadBooleanValue(ParamValue);
        'showWhenLess':
            spinShowWhenLess.Value := ReadIntegerValue(ParamValue);
        'showWhenLessInRandom':
            spinShowWhenLessInRandom.Value := ReadIntegerValue(ParamValue);
        'limitationTypeOfBattles': begin
            cboxLimitationTypeOfBattles.Checked := ReadBooleanValue(ParamValue);
            DisabledControls(cboxLimitationTypeOfBattles);
        end;
        'message':
            editMessage.Text := ReadStringValue(ParamValue);
        'useSquadChat': begin
            cboxUseSquadChat.Checked := ReadBooleanValue(ParamValue);
            DisabledControls(cboxUseSquadChat);
        end;
        'helpMeOption':
            cboxHelpMeOption.Checked := ReadBooleanValue(ParamValue);
        'cellClickOption':
            cboxCellClickOption.Checked := ReadBooleanValue(ParamValue);
        'timeEndMessage': begin
            spinTimeEndMessage.Value := ReadIntegerValue(ParamValue);
            DisabledControls(spinTimeEndMessage);
        end;
        'endMessage': begin
            editEndMessage.Text := ReadStringValue(ParamValue);
            DisabledControls(editEndMessage);
        end;
        'hotKey': begin
            Analyze_HotKey_Param(ParamValue);
        end;
        'enabledForVehType': begin
            Analyze_EnabledForVehType_Param(ParamValue);
        end;
    end;
end;

procedure TFormMain.btnSaveClick(Sender: TObject);
var
    Doc: TXMLDocument;
begin
    ReadXMLFile(Doc, XmlPath);

    TButton(Sender).Enabled := False;

    with Doc.DocumentElement do begin
        WriteValue(FindNode('disabledByStartBattle'), cboxDisabledByStartBattle.Checked);
        WriteValue(FindNode('limitationTypeOfBattles'), cboxLimitationTypeOfBattles.Checked);
        WriteValue(FindNode('showWhenLess'), spinShowWhenLess.Value);
        WriteValue(FindNode('showWhenLessInRandom'), spinShowWhenLessInRandom.Value);
        WriteValue(FindNode('message'), editMessage.Text);
        WriteValue(FindNode('useSquadChat'), cboxUseSquadChat.Checked);
        WriteValue(FindNode('helpMeOption'), cboxHelpMeOption.Checked);
        WriteValue(FindNode('cellClickOption'), cboxCellClickOption.Checked);
        WriteValue(FindNode('timeEndMessage'), spinTimeEndMessage.Value);
        WriteValue(FindNode('endMessage'), editEndMessage.Text);
        WriteValue(FindNode('hotKey'), Get_HotKey_Param);
        WriteValue(FindNode('enabledForVehType'), Get_EnabledForVehType_Param)
    end;

    with TCustomXMLWriter.Create do begin
        WriteNode(Doc);
        GetLines.SaveToFile(XmlPath);
    end;

    Doc.Free;
    TButton(Sender).Enabled := True;

    Application.MessageBox(PChar('Данные сохранены в файле ' + XML_NAME), PChar(Caption), MB_ICONINFORMATION);
end;

procedure TFormMain.combModKeyChange(Sender: TObject);
var
    ComboboxObject: TComboBox;
    i: Integer;
begin
    ComboboxObject := combKey;

    ComboboxObject.Items.Clear;
    if TComboBox(Sender).ItemIndex = 0 then
        for i in [9..12] do ComboboxObject.Items.Add('F' + IntToStr(i))
    else
        for i in [65..90] do ComboboxObject.Items.Add(Chr(i));
end;

procedure TFormMain.DisabledControls(Sender: TObject);
var
    Value: Boolean;
    SenderName: String;
begin
    SenderName := TComponent(Sender).Name;

    if (Sender is TCheckBox) then
        Value := TCheckBox(Sender).Checked
    else if (Sender is TSpinEdit) then
        Value := TSpinEdit(Sender).Value <> 0;

    case SenderName of
        'cboxLimitationTypeOfBattles': begin
            lblLimitationTypeOfBattlesHelp.Enabled := Value;
            cboxUseSquadChat.Enabled := not Value;
            lblUseSquadChatHelp.Enabled := not Value;
        end;
        'cboxEnableHotKey': begin
            combModKey.Enabled := Value;
            lblModKey.Enabled := Value;
            combKey.Enabled := Value;
            lblKey.Enabled := Value;
            lblEnableHotKeyHelp.Enabled := Value;
        end;
        'spinTimeEndMessage': begin
            lblEndMessageLabel.Enabled := Value;
            editEndMessage.Enabled := Value;
            lblEndMessageHelp.Enabled := Value;
            lblTimeEndMessageHelp.Enabled := Value;
        end;
    end;
end;

function TFormMain.ReadBooleanValue(Value: String): Boolean;
begin
    try
        Result := (LowerCase(Value) <> 'false') and (Value <> '0') and (Value <> '');
    except
        Result := False;
    end;
end;

function TFormMain.ReadIntegerValue(Value: String): Integer;
begin
    try
        Result := StrToInt(Value);
    except
        Result := 0;
    end;
end;

function TFormMain.ReadStringValue(Value: String): String;
begin
    try
        Result := Utf8Encode(Value);
    except
        Result := '';
    end;
end;

procedure TFormMain.WriteValue(Node: TDOMNode; Value: Boolean);
begin
    Node.FirstChild.NodeValue := BoolToStr(Value, '1', '0');
end;

procedure TFormMain.WriteValue(Node: TDOMNode; Value: Integer);
begin
    Node.FirstChild.NodeValue := IntToStr(Value);
end;

procedure TFormMain.WriteValue(Node: TDOMNode; Value: String);
begin
    Node.FirstChild.NodeValue := UTF8Decode(Value);
end;

procedure TFormMain.Analyze_HotKey_Param(ParamValue: String);
var
    HotkeyValue, ValueModKey, ValueKey: String;
    IndexModKey, IndexKey, PosPlus: Integer;
    isWrongData: Boolean;
begin
    HotkeyValue := ReadStringValue(ParamValue);
    HotkeyValue := StringReplace(HotkeyValue, KEY_PREFIX, '', [rfReplaceAll, rfIgnoreCase]);
    isWrongData := False;

    PosPlus := Pos('+', HotkeyValue);
    if PosPlus > 0 then begin
        ValueModKey := Copy(HotkeyValue, 1, PosPlus-1);
        ValueKey := Copy(HotkeyValue, PosPlus+1, Length(HotkeyValue));
        IndexModKey := combModKey.Items.IndexOf(ValueModKey);

        if IndexModKey >= 0 then begin
            combModKey.ItemIndex := IndexModKey;
            combModKeyChange(combModKey);
		end else begin
            isWrongData := True;
        end;
    end else
        ValueKey := HotkeyValue;

    if isWrongData = False then begin
        IndexKey := combKey.Items.IndexOf(ValueKey);
        if IndexKey >= 0 then
            combKey.ItemIndex := IndexKey
        else
            isWrongData := True;
    end;

    if isWrongData = True then begin
        combModKey.Items.Clear;
        combModKey.Items.Add(ValueModKey);
        combModKey.ItemIndex := 0;
        combModKey.Enabled := False;
        combKey.Items.Clear;
        combKey.Items.Add(ValueKey);
        combKey.ItemIndex := 0;
        combKey.Enabled := False;
    end;
end;

procedure TFormMain.Analyze_EnabledForVehType_Param(ParamValue: String);
var
    Values: TStringList;
begin
    try
        Values := TStringList.Create;
        Values.Delimiter := ',';
        Values.DelimitedText := ParamValue;

        cboxEnabledForHT.Checked := Values.IndexOf('HeavyTank') >= 0;
        cboxEnabledForMT.Checked := Values.IndexOf('MediumTank') >= 0;
        cboxEnabledForLT.Checked := Values.IndexOf('LightTank') >= 0;
        cboxEnabledForSPG.Checked := Values.IndexOf('SPG') >= 0;
        cboxEnabledForATSPG.Checked := Values.IndexOf('AT-SPG') >= 0;
    finally
        Values.Free;
    end;
end;

function TFormMain.Get_HotKey_Param: String;
begin
    Result := '';
    if combModKey.ItemIndex > 0 then
        Result := Result + {KEY_PREFIX +} combModKey.Text + '+';
    if combModKey.ItemIndex >= 0 then
        Result := Result + {KEY_PREFIX +} combKey.Text
    else
        Result := '';
end;

function TFormMain.Get_EnabledForVehType_Param: String;
var
    EnabledForVehTypeValue: TStringList;
begin
    Result := '';
    try
        EnabledForVehTypeValue := TStringList.Create;
        EnabledForVehTypeValue.Delimiter := ',';
        if cboxEnabledForHT.Checked then EnabledForVehTypeValue.Add('HeavyTank');
        if cboxEnabledForMT.Checked then EnabledForVehTypeValue.Add('MediumTank');
        if cboxEnabledForLT.Checked then EnabledForVehTypeValue.Add('LightTank');
        if cboxEnabledForATSPG.Checked then EnabledForVehTypeValue.Add('AT-SPG');
        if cboxEnabledForSPG.Checked then EnabledForVehTypeValue.Add('SPG');
        Result := StringReplace(EnabledForVehTypeValue.DelimitedText, ',', ', ', [rfReplaceAll]);
    finally
        EnabledForVehTypeValue.Free;
    end;
end;

end.

