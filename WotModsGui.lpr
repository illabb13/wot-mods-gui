program WotModsGui;

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}{$ENDIF}
    Interfaces, // this includes the LCL widgetset
    Forms, Unit_Main, Unit_CustomXMLWriter;

{$R *.res}

begin
    Application.Title := 'Observed Mod Configurator';
    RequireDerivedFormResource := True;
    Application.Initialize;
    Application.CreateForm(TFormMain, FormMain);
    Application.Run;
end.

