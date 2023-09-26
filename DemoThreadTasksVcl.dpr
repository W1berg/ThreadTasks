program DemoThreadTasksVcl;

uses
  Vcl.Forms,
  uDemos in 'uDemos.pas',
  uFormMain in 'Vcl\uFormMain.pas',
  uFrmThread in 'Vcl\uFrmThread.pas',
  uAes in '..\WibLib\uAes.pas',
  uArgon2 in '..\WibLib\uArgon2.pas',
  uArgon2Aes in '..\WibLib\uArgon2Aes.pas',
  uBase32 in '..\WibLib\uBase32.pas',
  uChaCha in '..\WibLib\uChaCha.pas',
  uClipboard in '..\WibLib\uClipboard.pas',
  uCrit in '..\WibLib\uCrit.pas',
  uData in '..\WibLib\uData.pas',
  uEventWrap in '..\WibLib\uEventWrap.pas',
  uFac in '..\WibLib\uFac.pas',
  uLock in '..\WibLib\uLock.pas',
  uLog in '..\WibLib\uLog.pas',
  uRnd in '..\WibLib\uRnd.pas',
  uRttiWrap in '..\WibLib\uRttiWrap.pas',
  uSync in '..\WibLib\uSync.pas',
  uTask in '..\WibLib\uTask.pas',
  uTaskCluster in '..\WibLib\uTaskCluster.pas',
  uTaskRunner in '..\WibLib\uTaskRunner.pas',
  uTaskStarter in '..\WibLib\uTaskStarter.pas',
  uTest in '..\WibLib\uTest.pas',
  uTypes in '..\WibLib\uTypes.pas',
  uWrap in '..\WibLib\uWrap.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;

end.
