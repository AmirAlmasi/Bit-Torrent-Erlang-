/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package gui;

import java.awt.Toolkit;

/**
 * 
 * @author Deno
 */
public class OperatingSystem {

	public static boolean windows() {
		String os = System.getProperty("os.name").toLowerCase();
		System.out.println(os);
		if (os.contains("windows")) {
			return true;
		} else {
			return false;
		}
	}

	public static boolean mac() {
		String os = System.getProperty("os.name").toLowerCase();
		if (os.contains("mac")) {
			return true;
		} else {
			return false;
		}
	}

	public static boolean unix() {
		String os = System.getProperty("os.name").toLowerCase();
		if (os.contains("unix") || os.contains("nux")) {
			return true;
		} else {
			return false;
		}
	}
}
