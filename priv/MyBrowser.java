package gui;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import chrriis.common.UIUtils;
import chrriis.dj.nativeswing.NativeSwing;
import chrriis.dj.nativeswing.swtimpl.NativeInterface;
import chrriis.dj.nativeswing.swtimpl.components.JWebBrowser;
import javax.swing.JInternalFrame;

/**
 * @author Christopher Deckers
 */
public class MyBrowser extends JPanel {
	static JPanel panel = new JPanel(new BorderLayout());
	static JInternalFrame frame = new JInternalFrame("DJ Native Swing Test");

	// static J frame = new JFrame("DJ Native Swing Test");
	public MyBrowser() {
		super(new BorderLayout());
		JPanel webBrowserPanel = new JPanel(new BorderLayout());
		webBrowserPanel.setBorder(BorderFactory
				.createTitledBorder("Native Web Browser component"));
		final JWebBrowser webBrowser = new JWebBrowser();
		webBrowser.navigate("http://www.erbit.org");
		webBrowserPanel.add(webBrowser, BorderLayout.CENTER);
		add(webBrowserPanel, BorderLayout.CENTER);
		// Create an additional bar allowing to show/hide the menu bar of the
		// web browser.

		webBrowser.setButtonBarVisible(false);
		webBrowser.setMenuBarVisible(false);
		webBrowser.setStatusBarVisible(false);
		webBrowser.setLocationBarVisible(false);
	}

	/* Standard main method to try that test as a standalone application. */
	public void start() {
		UIUtils.setPreferredLookAndFeel();
		NativeInterface.open();
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {

				frame.setDefaultCloseOperation(JInternalFrame.HIDE_ON_CLOSE);
				panel.add(new MyBrowser(), BorderLayout.CENTER);
				frame.add(panel, BorderLayout.CENTER);
				frame.setPreferredSize(panel.getSize());

				// frame.setLocationByPlatform(true);
				// frame.setVisible(false);
			}
		});
		NativeInterface.runEventPump();
	}

	// public static void main(String[] args){
	// start();
	// }

	public void webBrowserVisible() {

		ErBit.jScrollPane1.setVisible(false);
		ErBit.jPanel4.add(frame);
		frame.setSize(ErBit.jPanel4.getWidth(), ErBit.jPanel4.getHeight());
		frame.setVisible(true);

	}

}
