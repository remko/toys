import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.junit.Test;

import jyield.Continuable;
import jyield.Yield;


public class MergeBST {

	///////////////////////////////////////////////////////////////////////////////////////
	// Example 1: Merge lazy lists
	///////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Generate an infinite list of multiples of 'n'.
	 */
	@Continuable
	Iterable<Integer> multiplesOf(int n) {
		for (int i = n; ; i += n) {
			Yield.ret(i);		
		}
	}

	/**
	 * Merge infinite lists of multiples of 2 and multiples of 5
	 */
	@Test
	public void testMergeFirstTenOfInfiniteLists() {
		List<Integer> result = new ArrayList<Integer>();
		for (Integer i : merge(multiplesOf(2), multiplesOf(5))) {
			result.add(i);
			if (result.size() >= 10) {
				break;
			}
		}
		assertEquals(Arrays.asList(2, 4, 5, 6, 8, 10, 10, 12, 14, 15), result);
	}


	///////////////////////////////////////////////////////////////////////////////////////
	// Example 2: Flatten & Merge binary search trees
	///////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Tree datatype.
	 */
	class Node<T> {
		final Node<T> left;
		final Node<T> right;
		final T value;

		Node(T value) {
			this(value, null, null);
		}

		Node(T value, Node<T> left, Node<T> right) {
			this.value = value;
			this.left = left;
			this.right = right;
		}
	}

	/**
	 * Walk a tree in in-order, and yield each value
	 */
	@Continuable
	<T> Iterable<T> walk(Node<T> node) {
		if (node != null) {
			Yield.join(walk(node.left));
			Yield.ret(node.value);
			Yield.join(walk(node.right));
		}
		return null;
	}

	@Test
	public void testFlattenTreeToList() {
		Node<Integer> tree =
			new Node<Integer>(8,
					new Node<Integer>(4, new Node<Integer>(2), new Node<Integer>(5)),
					new Node<Integer>(12, new Node<Integer>(10), new Node<Integer>(16)));

		List<Integer> result = new ArrayList<Integer>();
		for (Integer i : walk(tree)) {
			result.add(i);
		}

		assertEquals(Arrays.asList(2, 4, 5, 8, 10, 12, 16), result);
	}

	@Test
	public void testMergeTreesToList() {
		Node<Integer> tree1 =
			new Node<Integer>(8,
					new Node<Integer>(4, new Node<Integer>(2), new Node<Integer>(5)),
					new Node<Integer>(12, new Node<Integer>(10), new Node<Integer>(16)));
		Node<Integer> tree2 =
			new Node<Integer>(9, 
					new Node<Integer>(5, new Node<Integer>(3), new Node<Integer>(6)),
					new Node<Integer>(13, new Node<Integer>(11), new Node<Integer>(17)));

		List<Integer> result = new ArrayList<Integer>();
		for (Integer i : merge(walk(tree1), walk(tree2))) {
			result.add(i);
		}

		assertEquals(Arrays.asList(2, 3, 4, 5, 5, 6, 8, 9, 10, 11, 12, 13, 16, 17 ), result);
	}


	///////////////////////////////////////////////////////////////////////////////////////
	// Common code 
	///////////////////////////////////////////////////////////////////////////////////////

	/**
	 * (Lazily) Merge 2 sorted lists into a sorted list.
	 */
	<T extends Comparable<T>> Iterable<T > merge(Iterable<T> listA, Iterable<T> listB) {
		Iterator<T> a = listA.iterator();
		Iterator<T> b = listB.iterator();
		T nextA = a.hasNext() ? a.next() : null;
		T nextB = b.hasNext() ? b.next() : null;						
		while (nextA != null || nextB != null) {
			if (nextA != null && (nextA.compareTo(nextB) <= 0 || nextB == null)) {
				Yield.ret(nextA);
				nextA = a.hasNext() ? a.next() : null;
			}
			else {
				Yield.ret(nextB);
				nextB = b.hasNext() ? b.next() : null;				
			}
		}
		return null;
	}
}
