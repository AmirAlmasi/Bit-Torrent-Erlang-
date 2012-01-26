/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package gui;

import java.util.Calendar;

/**
 * 
 * @author Deno
 */
public class Timer {
	long startTime;
	long stopTime;
	long size;

	public Timer() {
		Calendar cal = Calendar.getInstance();
		startTime = cal.getTimeInMillis();
		size = 0;
		System.out.println(startTime);
	}

	public void setTime() {
		Calendar cal = Calendar.getInstance();
		startTime = cal.getTimeInMillis();
		size = 0;
	}

	public long getTime() {
		Calendar cal = Calendar.getInstance();
		stopTime = cal.getTimeInMillis();
		return stopTime - startTime;
	}

	public void addData(long l) {
		size = size + l;
	}

	public long getData() {
		return size;
	}

}
